import pandas as pd
from pathlib import Path
import polars as pl
from tinygrad import Tensor, nn, TinyJit
from rich.progress import (
        BarColumn,
        MofNCompleteColumn,
        Progress,
        TextColumn,
        TimeElapsedColumn,
        TimeRemainingColumn,
    )


def load_data(path: Path) -> pl.LazyFrame:
    """Load data from a local file."""
    return pl.scan_csv(path)


def create_dates(data: pl.LazyFrame) -> pl.LazyFrame:
    data = data.collect().to_pandas()
    min_date = pd.to_datetime(data["timestamp"].min())
    max_date = pd.to_datetime(data["timestamp"].max())

    date_range = pd.date_range(start=min_date, end=max_date, freq="D")

    result = pd.DataFrame({
        "timestamp": date_range,
        "time_index": range(len(date_range))
    })

    result["timestamp"] = result["timestamp"].dt.date.astype(str)

    return pl.LazyFrame(result)


def get_min_max_time_index(data: pl.LazyFrame) -> tuple[int, int]:
    return (
        data.collect().to_pandas()["time_index"].min(),
        data.collect().to_pandas()["time_index"].max(),
    )


def filter_low_frequency_data(data: pl.LazyFrame, threshold: int = 50) -> pl.LazyFrame:
    return (
        data.group_by("ticker")
        .agg(pl.len().alias("count"))
        .filter(pl.col("count") >= threshold)
        .join(data, on="ticker", how="inner")
        .drop("count")
    )


def get_tickers(data: pl.LazyFrame) -> int:
    return data.collect().to_pandas()["ticker"].dropna().drop_duplicates().tolist()


def shape_tickers(data, tickers):
    progress_bar = Progress(
        TextColumn("[progress.percentage]{task.percentage:>3.0f}%"),
        BarColumn(),
        MofNCompleteColumn(),
        TextColumn("•"),
        TimeElapsedColumn(),
        TextColumn("•"),
        TimeRemainingColumn(),
    )

    with progress_bar:
        for ticker in progress_bar.track(tickers):
            tick = data.filter(pl.col("ticker") == ticker).with_columns([
                pl.col("time_index").cast(pl.Int32),
                pl.col("ticker").cast(pl.Utf8),
                pl.col("open").cast(pl.Float32),
                pl.col("high").cast(pl.Float32),
                pl.col("low").cast(pl.Float32),
                pl.col("close").cast(pl.Float32),
            ]).sort(["ticker", "time_index"])
            print(tick.collect())
            break




def join_data(data: pl.LazyFrame, dates: pl.LazyFrame) -> pl.LazyFrame:
    return (
        dates
            .join(data, on="timestamp", how="left")
            .with_columns([
                pl.col("time_index").cast(pl.Int32),
                pl.col("ticker").cast(pl.Utf8),
                pl.col("open").cast(pl.Float32),
                pl.col("high").cast(pl.Float32),
                pl.col("low").cast(pl.Float32),
                pl.col("close").cast(pl.Float32),
            ])
        .sort(["ticker", "time_index"])
    )


def drop_missing_tickers(data: pl.LazyFrame) -> pl.LazyFrame:
    return data.drop_nulls(["ticker"])

def train_test_split(data, frac: float =0.8) -> tuple[pl.DataFrame, pl.DataFrame]:
    shuffled = data.collect().sample(fraction=1, shuffle=True)
    observations = shuffled.shape[0]
    split = int(observations * frac)
    return shuffled.head(split), shuffled.tail(observations - split)


def dataframe_to_tensor(data: pl.DataFrame) -> Tensor:
    return Tensor(data)



class TemportalFusionTransformer:
    def __init__(self, n_tickers) -> None:
        self.ticker_embedding = nn.Embedding(n_tickers, 32)

    def __call__(self, x: Tensor, ticker_ids ) -> Tensor:
        return self.ticker_embedding(ticker_ids)


def train_model(X_train: Tensor, Y_train: Tensor, n_tickers: int):
    model = TemportalFusionTransformer(n_tickers)

    optim = nn.optim.Adam(nn.state.get_parameters(model))


    batch_size = 128
    def train_step():
        Tensor.training = True
        samples = Tensor.randint(batch_size, high=X_train.shape[0])
        X, Y = X_train[samples], Y_train[samples]
        optim.zero_grad()
        rmse = lambda x, y: (y - model(x)).pow(2).mean().sqrt()
        loss = rmse(X, Y).backward()

        optim.step()
        return loss

    jit_step = TinyJit(train_step)


    for step in range(100_000):
        loss = jit_step()
        if step % 100 == 0:
            print(f"step {step:4d}, loss {loss.item():.2f}")


def pipeline(path: Path) -> None:
    """Pipeline to train and save a price prediction model."""
    data = load_data(path)
    data = filter_low_frequency_data(data)
    dates = create_dates(data)

    unique_tickers = get_tickers(data)
    print(f"loaded {len(unique_tickers)} tickers")

    min_time_index, max_time_index = get_min_max_time_index(dates)
    print(f"min time index: {min_time_index}, max time index: {max_time_index}")

    data = join_data(data, dates)
    data = drop_missing_tickers(data)

    train, test = train_test_split(data)
    print(train.shape)
    print(test.shape)

    train_x = dataframe_to_tensor(train.to_pandas()[["open"]].values)
    train_y = dataframe_to_tensor(train.to_pandas()[["close"]].values)

    test_x = dataframe_to_tensor(test.to_pandas()[["open"]].values)
    test_y = dataframe_to_tensor(test.to_pandas()[["close"]].values)

    model = train_model(train_x, train_y, len(unique_tickers))

    # shape_tickers(data, unique_tickers)


if __name__ == "__main__":
    pipeline(Path("data/previous-close/full_tickers.csv"))
