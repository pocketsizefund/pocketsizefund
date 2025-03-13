from typing import Dict, List, Iterable
from tinygrad import Tensor
from tinygrad.dtype import dtypes
import polars as pl
from category_encoders import OrdinalEncoder
from typing import Tuple
import numpy as np  # NOTE: remove


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
        scalers: Dict[str, Dict[str, Tensor]] = {},
    ) -> None:
        self.batch_size = batch_size
        self.sequence_length = sequence_length
        self.scalers = scalers
        self.preprocessors = {}

    def load_data(self, data: pl.DataFrame) -> None:
        data = data.with_columns(
            [
                pl.col("timestamp").str.strptime(pl.Datetime).cast(pl.Date),
                pl.col("open_price").cast(pl.Float32).alias("open_price"),
                pl.col("high_price").cast(pl.Float32).alias("high_price"),
                pl.col("low_price").cast(pl.Float32).alias("low_price"),
                pl.col("close_price").cast(pl.Float32).alias("close_price"),
                pl.col("volume").cast(pl.Float32),
                pl.col("volume_weighted_average_price").cast(pl.Float32),
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

        ticker_series = data["ticker"].to_pandas()  # NOTE: sklearn warning source
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

                ticker_key = ticker[0]

                self.scalers[ticker_key] = {
                    "means": Tensor(means.to_numpy().flatten(), requires_grad=False).cast(
                        dtypes.float32
                    ),
                    "standard_deviations": Tensor(
                        standard_deviations.to_numpy().flatten(), requires_grad=False
                    ).cast(dtypes.float32),
                }

        groups = []
        groups: List[Tensor] = []
        for ticker, group in data.group_by("ticker"):
            ticker_key = ticker[0]

            means = self.scalers[ticker_key]["means"].cast(dtypes.float32)
            standard_deviations = self.scalers[ticker_key]["standard_deviations"].cast(
                dtypes.float32
            )

            group_data = Tensor(group.select(continuous_variable_columns).to_numpy()).cast(
                dtypes.float32
            )

            scaled_group = (group_data - means) / standard_deviations

            num_rows = scaled_group.shape[0]

            ticker_int = Tensor.full((num_rows, 1), ticker_key).cast(dtypes.float32)

            scaled_group_with_ticker = scaled_group.cat(ticker_int, dim=1)

            groups.append(scaled_group_with_ticker)

        # NOTE: handle cases w/ < 2 groups
        output_data = Tensor.cat(*groups, dim=0).cast(dtypes.float32).realize()

        print("dataset - output_data:", output_data.shape)  # TEMP

        self.sample_count = output_data.shape[0]

        self.data = output_data

    def get_preprocessors(self):
        if self.preprocessors is None or len(self.preprocessors) == 0:
            raise ValueError("Preprocessors attribute has not been set")

        means_by_ticker = {ticker: values["means"] for ticker, values in self.scalers.items()}
        standard_deviations_by_ticker = {
            ticker: values["standard_deviations"] for ticker, values in self.scalers.items()
        }

        return {
            "means_by_ticker": means_by_ticker,
            "standard_deviations_by_ticker": standard_deviations_by_ticker,
            "ticker_encoder": self.preprocessors["ticker_encoder"],
            "indices": self.preprocessors["indices"],
        }

    def __iter__(self) -> Iterable[Tuple[Tensor, Tensor, Tensor]]:
        close_price_index = self.preprocessors["indices"]["close_price"]
        prediction_horizon = 5  # NOTE: change to dynamic reference

        # NOTE: change "step" to variable here
        # NOTE: clean this all up
        for i in range(
            0, self.sample_count - self.sequence_length - prediction_horizon + 1, self.batch_size
        ):
            batch_data = self.data[
                i : i + self.batch_size + self.sequence_length + prediction_horizon
            ]
            if batch_data.shape[0] < self.batch_size + self.sequence_length + prediction_horizon:
                padding = Tensor.zeros(
                    (
                        self.batch_size
                        + self.sequence_length
                        + prediction_horizon
                        - batch_data.shape[0],
                        *batch_data.shape[1:],
                    )
                ).realize()
                batch_data = Tensor(batch_data).stack(padding, dim=0).realize()

            tickers = batch_data[: self.batch_size, -1].cast(dtypes.int32).realize()
            feature_indices = [
                self.preprocessors["indices"][col] for col in continuous_variable_columns
            ]
            historical_features = Tensor.stack(
                *[
                    batch_data[i : i + self.sequence_length, feature_indices]
                    for i in range(self.batch_size)
                ],
                dim=0,
            ).realize()

            targets = Tensor.stack(
                *[
                    batch_data[
                        i + self.sequence_length : i + self.sequence_length + prediction_horizon,
                        close_price_index,
                    ]
                    for i in range(self.batch_size)
                ],
                dim=0,
            ).realize()

            yield tickers, historical_features, targets
