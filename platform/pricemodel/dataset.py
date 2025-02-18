from typing import Dict, List, Iterable
from tinygrad import Tensor
import polars as pl
from category_encoders import OrdinalEncoder
import numpy as np
from typing import Tuple


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
        sequence_length: int,
        sample_count: int,
        scalers: Dict[str, Dict[str, Tensor]] = {},
    ) -> None:
        self.batch_size = batch_size
        self.sequence_length = sequence_length
        self.sample_count = sample_count
        self.scalers = scalers
        self.preprocessors = {}

    def load_data(self, data: pl.DataFrame) -> None:
        data = data.with_columns(
            [
                pl.col("timestamp").str.strptime(pl.Datetime).cast(pl.Date),
                pl.col("open_price").cast(pl.Float64).alias("open_price"),
                pl.col("high_price").cast(pl.Float64).alias("high_price"),
                pl.col("low_price").cast(pl.Float64).alias("low_price"),
                pl.col("close_price").cast(pl.Float64).alias("close_price"),
                pl.col("volume").cast(pl.Float64),
                pl.col("volume_weighted_average_price").cast(pl.Float64),
                pl.col("ticker").cast(pl.Utf8),
            ]
        )

        self.preprocessors["indices"] = {col: idx for idx, col in enumerate(data.columns)}

        data = data.unique(subset=["ticker", "timestamp"])

        tickers = data.select("ticker").unique()
        minimum_date = data.select(pl.col("timestamp").min())[0, 0]
        maximum_date = data.select(pl.col("timestamp").max())[0, 0]

        full_dates = pl.DataFrame(
            {
                "timestamp": pl.date_range(
                    start=minimum_date,
                    end=maximum_date,
                    interval="1d",
                    closed="both",
                    eager=True,
                )
            }
        )

        full_tickers_and_dates = tickers.join(full_dates, how="cross")

        full_tickers_and_dates = full_tickers_and_dates.with_columns(
            [pl.col("timestamp").rank(method="dense").cast(pl.Int32).alias("time_index") - 1]
        )

        data = full_tickers_and_dates.join(data, on=["ticker", "timestamp"], how="left")

        data = data.sort(["ticker", "timestamp"])

        data = data.group_by("ticker").map_groups(
            lambda df: df.sort("timestamp").with_columns(
                [
                    pl.col(col)
                    .interpolate()
                    .fill_null(strategy="forward")
                    .fill_null(strategy="backward")
                    for col in (continuous_variable_columns)
                ]
            )
        )

        ticker_series = data["ticker"].to_pandas()
        ticker_encoder = OrdinalEncoder(
            cols=["ticker"],
            handle_unknown="use_encoded_value",
            handle_missing="use_encoded_value",
        )
        self.preprocessors["ticker_encoder"] = ticker_encoder
        encoded_tickers = ticker_encoder.fit_transform(ticker_series)

        data = data.with_columns(pl.Series("ticker", encoded_tickers["ticker"]))

        if self.scalers is None or len(self.scalers) == 0:
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

        self.data = output_data

    def get_preprocessors(self):
        if self.preprocessors is None or len(self.preprocessors) == 0:
            raise ValueError("Preprocessors attribute has not been set")

        means_by_ticker = ({ticker: values["means"] for ticker, values in self.scalers.items()},)
        standard_deviations_by_ticker = (
            {ticker: values["standard_deviations"] for ticker, values in self.scalers.items()},
        )

        return {
            "means_by_ticker": means_by_ticker,
            "standard_deviations_by_ticker": standard_deviations_by_ticker,
            "ticker_encoder": self.preprocessors["ticker_encoder"],
            "indices": self.preprocessors["indices"],
        }

    def __iter__(self) -> Iterable[Tuple[Tensor, Tensor, Tensor]]:
        close_price_idx = self.preprocessors["indices"]["close_price"]

        for i in range(0, self.sample_count, self.batch_size):
            batch_data = self.data[i : i + self.batch_size + self.sequence_length]

            if batch_data.shape[0] < self.batch_size + self.sequence_length:
                padding = Tensor.zeros(
                    (
                        self.batch_size + self.sequence_length - batch_data.shape[0],
                        *batch_data.shape[1:],
                    )
                )
                batch_data = Tensor(batch_data).stack(padding, dim=0)

            tickers = batch_data[: self.batch_size, 0]

            historical_features = Tensor.stack(
                *[batch_data[i : i + self.sequence_length, 1:] for i in range(self.batch_size)],
                dim=0,
            )

            targets = batch_data[: self.batch_size, close_price_idx].reshape(self.batch_size, 1)

            yield tickers, historical_features, targets
