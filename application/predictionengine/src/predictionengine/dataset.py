from collections.abc import Generator
from typing import Any

import polars as pl
from category_encoders import OrdinalEncoder
from tinygrad.tensor import Tensor

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
        scalers: dict[str, dict[str, Tensor]] | None = None,
    ) -> None:
        self.batch_size: int = batch_size
        self.sequence_length: int = sequence_length
        self.sample_count: int = sample_count
        self.scalers: dict[str, dict[str, Tensor]] = scalers or {}
        self.preprocessors: dict[str, Any] = {}

    def __len__(self) -> int:
        return (self.sample_count + self.batch_size - 1) // self.batch_size

    def _cast_columns(self, data: pl.DataFrame) -> pl.DataFrame:
        return data.with_columns(
            [
                pl.col("timestamp").str.strptime(pl.Datetime).cast(pl.Date),
                pl.col("open_price").cast(pl.Float64),
                pl.col("high_price").cast(pl.Float64),
                pl.col("low_price").cast(pl.Float64),
                pl.col("close_price").cast(pl.Float64),
                pl.col("volume").cast(pl.Float64),
                pl.col("volume_weighted_average_price").cast(pl.Float64),
                pl.col("ticker").cast(pl.Utf8),
            ]
        )

    def _generate_complete_time_series(self, data: pl.DataFrame) -> pl.DataFrame:
        data = data.unique(subset=["ticker", "timestamp"])

        tickers = data.select("ticker").unique()
        minimum_timestamp = data.select(pl.col("timestamp").min())[0, 0]
        maximum_timestamp = data.select(pl.col("timestamp").max())[0, 0]

        full_dates = pl.DataFrame(
            {
                "timestamp": pl.date_range(
                    start=minimum_timestamp,
                    end=maximum_timestamp,
                    interval="1d",
                    closed="both",
                    eager=True,
                )
            }
        )

        full_tickers_and_dates = tickers.join(full_dates, how="cross")

        full_tickers_and_dates = full_tickers_and_dates.with_columns(
            [
                pl.col("timestamp")
                .rank(method="dense")
                .cast(pl.Int32)
                .alias("time_index")
                - 1
            ]
        )

        data = full_tickers_and_dates.join(data, on=["ticker", "timestamp"], how="left")
        data = data.sort(["ticker", "timestamp"])

        return data.group_by("ticker").map_groups(
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

    def _encode_tickers(self, data: pl.DataFrame) -> pl.DataFrame:
        ticker_encoder = OrdinalEncoder(
            cols=["ticker"],
            handle_unknown="use_encoded_value",
            handle_missing="use_encoded_value",
        )
        self.preprocessors["ticker_encoder"] = ticker_encoder

        ticker_df = data.select("ticker").to_pandas()
        encoded_tickers = ticker_encoder.fit_transform(ticker_df)

        return data.with_columns(pl.Series("ticker", encoded_tickers["ticker"]))

    def _compute_scalers(self, data: pl.DataFrame) -> None:
        if len(self.scalers) == 0:
            self.scalers: dict[str, dict[str, Tensor]] = {}
            for ticker_key, group in data.group_by("ticker"):
                ticker = ticker_key[0]
                means = group[continuous_variable_columns].mean()
                standard_deviations = group[continuous_variable_columns].std()

                self.scalers[str(ticker)] = {
                    "means": Tensor(means.to_numpy()),
                    "standard_deviations": Tensor(standard_deviations.to_numpy()),
                }

    def _scale_data(self, data: pl.DataFrame) -> Tensor:
        groups: list[Tensor] = []
        for ticker_key, group in data.group_by("ticker"):
            ticker = ticker_key[0]
            means = self.scalers[str(ticker)]["means"]
            standard_deviations = self.scalers[str(ticker)]["standard_deviations"]

            ticker_column = Tensor(group.select("ticker").to_numpy())
            group_data = Tensor(group.select(continuous_variable_columns).to_numpy())

            scaled_group = (group_data.sub(means)).div(standard_deviations)

            combined_group = ticker_column.cat(scaled_group, dim=1)
            groups.append(combined_group)

        if not groups:
            message = "No data available after preprocessing"
            raise ValueError(message)

        output_data = Tensor.empty(groups[0].shape)
        return output_data.cat(*groups, dim=0)

    def load_data(self, data: pl.DataFrame) -> None:
        data = self._cast_columns(data)

        self.preprocessors["indices"] = {
            col: idx for idx, col in enumerate(data.columns)
        }

        data = self._generate_complete_time_series(data)
        data = self._encode_tickers(data)
        self._compute_scalers(data)
        self.data = self._scale_data(data)

    def get_preprocessors(self) -> dict[str, Any]:
        if not self.preprocessors:
            message = "Preprocessors have not been initialized."
            raise ValueError(message)

        means_by_ticker = {
            ticker: values["means"] for ticker, values in self.scalers.items()
        }
        standard_deviations_by_ticker = {
            ticker: values["standard_deviations"]
            for ticker, values in self.scalers.items()
        }

        return {
            "means_by_ticker": means_by_ticker,
            "standard_deviations_by_ticker": standard_deviations_by_ticker,
            "ticker_encoder": self.preprocessors["ticker_encoder"],
            "indices": self.preprocessors["indices"],
        }

    def batches(self) -> Generator[tuple[Tensor, Tensor, Tensor], None, None]:
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

                batch_data = batch_data.cat(padding, dim=0)

            tickers = batch_data[: self.batch_size, 0]

            batch_tensors = [
                batch_data[i : i + self.sequence_length, 1:]
                for i in range(self.batch_size)
            ]

            if not batch_tensors:
                message = "Cannot stack empty batch tensors (batch_size must be ≥ 1)"
                raise ValueError(message)
            if len(batch_tensors) == 1:
                historical_features = batch_tensors[0].unsqueeze(0)
            else:
                historical_features = Tensor.stack(
                    batch_tensors[0],
                    *batch_tensors[1:],
                    dim=0,
                )

            targets = batch_data[: self.batch_size, close_price_idx].reshape(
                self.batch_size, 1
            )

            yield tickers, historical_features, targets
