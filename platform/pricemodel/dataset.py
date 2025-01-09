import polars as pl
import category_encoders as ce
from tinygrad import Tensor
from typing import Iterable, Dict, List
import warnings

warnings.filterwarnings("ignore", category=FutureWarning, message=".*_get_tags.*")

continuous_variable_columns = [
    "open_price",
    "high_price",
    "low_price",
    "close_price",
    "volume",
    "volume_weighted_average_price",
]


class DataSet:
    def __init__(
        self,
        batch_size: int,
        sample_count: int,
        scalers: Dict[str, Dict[str, Tensor]] = None,
    ) -> None:
        self.batch_size = batch_size
        self.sample_count = sample_count
        self.scalers = scalers
        self.ticker_encoder = None

    def set_data(self, data: pl.DataFrame) -> None:
        data = data.with_columns(
            [
                pl.col("timestamp").str.strptime(pl.Datetime).cast(pl.Date),  # , "%Y-%m-%d"),
                pl.col("open_price").cast(pl.Float64).alias("open_price"),
                pl.col("high_price").cast(pl.Float64).alias("high_price"),
                pl.col("low_price").cast(pl.Float64).alias("low_price"),
                pl.col("close_price").cast(pl.Float64).alias("close_price"),
                pl.col("volume").cast(pl.Float64),
                pl.col("volume_weighted_average_price").cast(pl.Float64),
                pl.col("ticker").cast(pl.Utf8),
            ]
        )

        data = data.unique(subset=["ticker", "timestamp"])

        tickers = data.select("ticker").unique()
        min_date = data.select(pl.col("timestamp").min())[0, 0]
        max_date = data.select(pl.col("timestamp").max())[0, 0]

        full_dates = pl.DataFrame(
            {
                "timestamp": pl.date_range(
                    start=min_date,
                    end=max_date,
                    interval="1d",
                    closed="both",
                    eager=True,
                )
            }
        )

        full_df = tickers.join(full_dates, how="cross")

        full_df = full_df.with_columns(
            [pl.col("timestamp").rank(method="dense").cast(pl.Int32).alias("time_idx") - 1]
        )

        data = full_df.join(data, on=["ticker", "timestamp"], how="left")

        data = data.sort(["ticker", "timestamp"])

        data = data.with_columns(pl.col("timestamp").dt.weekday().alias("day_of_week"))

        data = data.group_by("ticker").map_groups(
            lambda df: df.sort("timestamp").with_columns(
                [
                    pl.col(col)
                    .interpolate()
                    .fill_null(strategy="forward")
                    .fill_null(strategy="backward")
                    for col in (continuous_variable_columns + ["day_of_week"])
                ]
            )
        )

        ticker_series = data["ticker"].to_pandas()
        ticker_encoder = ce.OrdinalEncoder(
            cols=["ticker"],
            handle_unknown="use_encoded_value",
            handle_missing="use_encoded_value",
        )
        self.ticker_encoder = ticker_encoder
        encoded_tickers = ticker_encoder.fit_transform(ticker_series)

        data = data.with_columns(pl.Series("ticker", encoded_tickers["ticker"]))

        if self.scalers is None:
            self.scalers: Dict[str, Dict[str, Tensor]] = {}
            for ticker, group in data.group_by("ticker"):
                means = group[continuous_variable_columns].mean()
                standard_deviations = group[continuous_variable_columns].std()

                self.scalers[ticker] = {
                    "means": Tensor(means.to_numpy()),
                    "standard_deviations": Tensor(standard_deviations.to_numpy()),
                }

        groups: List[Tensor] = []
        for ticker, group in data.group_by("ticker"):
            means = self.scalers[ticker]["means"]
            standard_deviations = self.scalers[ticker]["standard_deviations"]

            group_data = Tensor(group.select(continuous_variable_columns).to_numpy())

            scaled_group = (group_data.sub(means)).div(standard_deviations)

            groups.append(scaled_group)

        output_data = Tensor.empty(groups[0].shape)
        output_data = output_data.cat(*groups, dim=0)

        self.data = output_data.numpy()  # FIX

    def get_information(self) -> Dict[str, Tensor]:
        if self.scalers is None:
            raise ValueError("Scalers attribute has not been set")

        if self.data is None:
            raise ValueError("Data attribute has not been set")

        means_by_ticker = ({ticker: values["means"] for ticker, values in self.scalers.items()},)
        standard_deviations_by_ticker = (
            {ticker: values["standard_deviations"] for ticker, values in self.scalers.items()},
        )

        return {
            "means_by_ticker": means_by_ticker,
            "standard_deviations_by_ticker": standard_deviations_by_ticker,
            "encoder_lengths": Tensor([30] * len(self.data)),
            "decoder_lengths": Tensor([5] * len(self.data)),
            "encoder_categories": Tensor(self.data[["ticker"]].values[:30]),
            "decoder_categories": Tensor(self.data[["ticker"]].values[-5:]),
            "encoder_continous_variables": Tensor(
                self.data[continuous_variable_columns].values[:30]
            ),
            "decoder_continous_variables": Tensor(
                self.data[continuous_variable_columns].values[-7:]
            ),
        }

    def __iter__(self) -> Iterable[Tensor]:
        for i in range(0, self.sample_count, self.batch_size):
            batch = self.data[i : i + self.batch_size]
            if batch.shape[0] < self.batch_size:
                padding = Tensor.zeros((self.batch_size - batch.shape[0], *batch.shape[1:]))
                # batch = Tensor(data=batch.to_numpy())
                batch = batch.stack(padding, dim=0)

            yield Tensor(data=batch.to_numpy())


# NOTE: testing

dataset = DataSet(batch_size=32, sample_count=1000)

dataframe = pl.read_csv("platform/pricemodel/consolidated_data.csv")

dataset.set_data(dataframe)

information = dataset.get_information()

print(information)
