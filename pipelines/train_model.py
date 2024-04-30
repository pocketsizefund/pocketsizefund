import polars as pl
from prefect import flow, task
from prefect_ray.task_runners import RayTaskRunner

from pipelines import lstm, transformations
from pipelines.loaders import load_dataframe
from pipelines.types import Bucket, ColumnSubset, TimeWindow


@task
def valid_size(data, window: TimeWindow):
    return data.shape[0] >= window.length


@task
def train_val_test_split(
    data: pl.DataFrame,
    splits: tuple[int, int, int],
    preserve_order: bool = True,
):
    if not preserve_order:
        data = data.shuffle()

    total_rows = data.shape[0]

    first_split = int(total_rows * splits[0])
    second_split = int(total_rows * splits[1])

    return (
        data.head(first_split),
        data.slice(first_split, first_split + second_split),
        data.tail(total_rows - first_split - second_split),
    )


@flow
def train_ticker(
    *,
    data: pl.DataFrame,
    ticker: str,
    close_price_index: int,
    features: list[str],
    train_test_splits: tuple[float, float, float],
    window: TimeWindow,
):
    ticker_data = transformations.filter_data_by_column_value(
        data,
        column="ticker",
        value=ticker,
    )

    if not valid_size(ticker_data, window=window):
        requires = window.input + window.output
        msg = f"{ticker_data=} has insufficient observations, {requires=}"
        raise ValueError(msg)

    train, val, test = train_val_test_split(
        ticker_data, splits=train_test_splits)

    train = transformations.sort_by_columns(
        transformations.min_max_scaler(train),
        ["timestamp"],
    )
    train = lstm.shape_timeseries_dataset(
        data=train,
        features=features,
        window=window,
        close_price_index=close_price_index,
    )

    val = transformations.sort_by_columns(
        transformations.min_max_scaler(val),
        ["timestamp"],
    )
    val = lstm.shape_timeseries_dataset(
        data=val,
        window=window,
        features=features,
        close_price_index=close_price_index,
    )

    test = transformations.sort_by_columns(
        transformations.min_max_scaler(test),
        ["timestamp"],
    )

    test = lstm.shape_timeseries_dataset(
        data=test,
        window=window,
        features=features,
        close_price_index=close_price_index,
    )

    model = lstm.create_model(3, window)
    model = lstm.train_model(model, train, val)

    lstm.save_model(model)

    lstm.evaluate_model(model, test)


@flow
def pipeline(
    data_path: Bucket,
    timestamp_field: str,
    ticker_field: str,
    features: ColumnSubset,
    close_price_index: float,
    train_test_splits: tuple[float, float, float],
    task_runner=RayTaskRunner(),  # noqa: B008
):
    data = load_dataframe(data_path)
    data = transformations.drop_nulls(data)
    data = transformations.drop_duplicates(
        data, subset=[timestamp_field, ticker_field])
    data = transformations.select_columns(
        data,
        subset=[timestamp_field, ticker_field] + features,
    )

    unique_tickers = transformations.select_columns(
        transformations.drop_duplicates(data, subset=[ticker_field]),
        subset=[ticker_field],
    ).to_dict()["ticker"]

    print(f"{unique_tickers=}")

    for ticker in unique_tickers:
        window = TimeWindow(input=5, output=3)
        train_ticker(
            data=data,
            ticker=ticker,
            close_price_index=close_price_index,
            train_test_splits=train_test_splits,
            features=features,
            window=window,
        )


if __name__ == "__main__":
    pipeline(
        data_path=Bucket(block="pocketsizefund-data-bucket",
                         prefix="tests", key="test_data.csv"),
        timestamp_field="timestamp",
        ticker_field="ticker",
        features=[
            "open_price",
            "high_price",
            "low_price",
            "close_price",
            "volume",
        ],
        close_price_index=3,
        train_test_splits=(0.7, 0.2, 0.1),
    )
