from typing import Dict, List, Iterable
from tinygrad import Tensor
from tinygrad.dtype import dtypes
import polars as pl
from category_encoders import OrdinalEncoder
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
        # TODO: remove "batch_size" parameter + calculate internally
        batch_size: int,  # number of tickers (e.g. 20)
        sequence_length: int,  # historic days (e.g. 30)
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
                    "means": Tensor(means.to_numpy().flatten()).cast(dtypes.float32),
                    "standard_deviations": Tensor(standard_deviations.to_numpy().flatten()).cast(
                        dtypes.float32
                    ),
                }

        self.data: Dict[str, Tensor] = {}
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

            ticker_int = Tensor.full((scaled_group.shape[0], 1), ticker_key).cast(dtypes.float32)

            self.data[ticker_key] = scaled_group.cat(ticker_int, dim=1)

    def get_preprocessors(self) -> Dict[str, Dict[str, Tensor]]:
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
        prediction_horizon = 5  # TODO: make parameter
        ticker_keys = list(self.data.keys())

        min_length = self.sequence_length + prediction_horizon
        valid_groups = {k: g for k, g in self.data.items() if g.shape[0] >= min_length}

        if len(valid_groups) < self.batch_size:  # TODO: make into equals check
            raise ValueError(
                f"Not enough tickers with sufficient data: {len(valid_groups)} < {self.batch_size}"
            )  # TODO: update error message

        tickers: Tensor = Tensor(ticker_keys, dtype=dtypes.int32).realize()
        features: List[Tensor] = []
        targets: List[Tensor] = []

        # TODO: rename all variables
        for ticker_key in ticker_keys:
            group_data = self.data[ticker_key]

            feat = group_data[-min_length:-prediction_horizon, :-1]  # exclude ticker column
            features.append(feat)

            targ = group_data[-prediction_horizon:, close_price_index]

            targ_quantiles = Tensor.stack(*[targ * 0.9, targ, targ * 1.1], dim=-1)
            targets.append(targ_quantiles)

        historical_features = Tensor.stack(*features, dim=0).realize()  # [20, 30, 6]
        targets = Tensor.stack(*targets, dim=0).realize()  # [20, 5, 3]

        yield tickers, historical_features, targets
