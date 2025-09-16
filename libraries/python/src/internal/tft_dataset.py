from datetime import date, datetime, timedelta

import pandera.polars as pa
import polars as pl
from tinygrad.tensor import Tensor


class Scaler:
    def __init__(self) -> None:
        pass

    def fit(self, data: pl.DataFrame) -> None:
        self.means = data.mean()
        self.standard_deviations = data.std()
        self.standard_deviations = self.standard_deviations.select(
            pl.all().replace(0, 1e-8)
        )  # avoid division by zero

    def transform(self, data: pl.DataFrame) -> pl.DataFrame:
        return (data - self.means) / self.standard_deviations

    def inverse_transform(self, data: pl.DataFrame) -> pl.DataFrame:
        return data * self.standard_deviations + self.means


class TFTDataset:
    """Temporal fusion transformer dataset preprocessing and postprocessing."""

    def __init__(self) -> None:
        pass

    def preprocess_and_set_data(self, data: pl.DataFrame) -> None:
        data = data.clone()

        raw_columns = (
            "ticker",
            "timestamp",
            "open_price",
            "high_price",
            "low_price",
            "close_price",
            "volume",
            "volume_weighted_average_price",
            "sector",
            "industry",
        )

        if set(data.columns) != set(raw_columns):
            message = f"Expected columns {raw_columns} but got {data.columns}"
            raise ValueError(message)

        self.continuous_columns = [
            "open_price",
            "high_price",
            "low_price",
            "close_price",
            "volume",
            "volume_weighted_average_price",
            "daily_return",
        ]

        self.categorical_columns = [
            "day_of_week",
            "day_of_month",
            "day_of_year",
            "month",
            "year",
            "is_holiday",
        ]

        self.static_categorical_columns = [
            "ticker",
            "sector",
            "industry",
        ]

        data = data.with_columns(
            pl.col("timestamp")
            .cast(pl.Datetime(time_unit="ms"))
            .dt.date()
            .alias("date"),
            pl.lit(False).alias("is_holiday"),  # noqa: FBT003
        )

        tickers = data.select(pl.col("ticker").unique())

        minimum_date: date = data.select(pl.col("date").min()).item()
        maximum_date: date = data.select(pl.col("date").max()).item()

        dates = pl.DataFrame(
            {
                "date": pl.date_range(
                    minimum_date,
                    maximum_date,
                    "1d",
                    eager=True,
                )
            }
        )

        dates_and_tickers = tickers.join(dates, how="cross")

        data = dates_and_tickers.join(data, on=["ticker", "date"], how="left")

        friday_number = 4

        # set is_holiday value for missing weekdays
        data = (
            data.with_columns(pl.col("date").dt.weekday().alias("temporary_weekday"))
            .with_columns(
                pl.when(
                    pl.col("is_holiday").is_null()
                    & (pl.col("temporary_weekday") <= friday_number)
                )
                .then(True)  # noqa: FBT003
                .when(
                    pl.col("is_holiday").is_null()
                    & (pl.col("temporary_weekday") > friday_number)
                )
                .then(False)  # noqa: FBT003
                .otherwise(pl.col("is_holiday"))  # keep existing values
                .alias("is_holiday")
            )
            .drop("temporary_weekday")
        )

        data = data.unique(subset=["ticker", "timestamp"])

        # ensure all rows have values instead of nulls
        data = data.with_columns(
            [
                pl.col("open_price").fill_null(0.0),
                pl.col("high_price").fill_null(0.0),
                pl.col("low_price").fill_null(0.0),
                pl.col("close_price").fill_null(0.0),
                pl.col("volume").fill_null(0.0),
                pl.col("volume_weighted_average_price").fill_null(0.0),
                pl.col("sector").fill_null("Not Available"),
                pl.col("industry").fill_null("Not Available"),
                pl.col("ticker").fill_null("UNKNOWN"),
                pl.col("timestamp").fill_null(
                    pl.col("date")
                    .cast(pl.Datetime)
                    .dt.replace_time_zone("UTC")
                    .cast(pl.Int64)
                    .floordiv(1000)
                ),
            ]
        )

        # compute new calendar columns
        data = data.with_columns(
            pl.col("date").dt.weekday().alias("day_of_week").cast(pl.Int64),
            pl.col("date").dt.day().alias("day_of_month").cast(pl.Int64),
            pl.col("date").dt.ordinal_day().alias("day_of_year").cast(pl.Int64),
            pl.col("date").dt.month().alias("month").cast(pl.Int64),
            pl.col("date").dt.year().alias("year").cast(pl.Int64),
        )

        # add time index column
        data = data.sort(["ticker", "timestamp"]).with_columns(
            pl.col("timestamp")
            .rank("dense")
            .over("ticker")
            .cast(pl.Int64)
            .alias("time_idx")
        )

        data = data.with_columns(
            pl.col("close_price").pct_change().over("ticker").alias("daily_return")
        )

        data = data.filter(pl.col("daily_return").is_not_null())

        data = dataset_schema.validate(data)

        self.scaler = Scaler()

        self.scaler.fit(data[self.continuous_columns])

        data = data.with_columns(  # scale continuous columns
            *[
                (pl.col(col) - self.scaler.means[col])
                / self.scaler.standard_deviations[col]
                for col in self.continuous_columns
            ]
        )

        mapping_columns = [
            "ticker",
            "sector",
            "industry",
            "is_holiday",
        ]

        mappings: dict[str, dict[str, int]] = {}

        for column in mapping_columns:
            data, mapping = self._create_mapping_and_encoding(data, column)
            mappings[column] = mapping

        self.mappings = mappings

        self.data = data

    def _create_mapping_and_encoding(
        self,
        data: pl.DataFrame,
        column: str,
    ) -> tuple[pl.DataFrame, dict]:
        unique_values = data[column].unique().to_list()

        mapping = {val: idx for idx, val in enumerate(unique_values)}

        data = data.with_columns(
            pl.col(column).replace(mapping).cast(pl.Int32).alias(column)
        )

        return data, mapping

    def _get_training_and_validation_data(
        self,
        validation_split: float = 0.8,
    ) -> tuple[pl.DataFrame, pl.DataFrame]:
        if validation_split in {0.0, 1.0}:
            message = "Validation split must be between 0.0 and 1.0 (exclusive)."
            raise ValueError(message)

        minimum_date: date = self.data.select(self.data["date"].min()).item()
        maximum_date: date = self.data.select(self.data["date"].max()).item()

        time_difference = maximum_date - minimum_date
        split_date = minimum_date + (time_difference * validation_split)

        training_data = self.data.filter(pl.col("date") <= split_date)
        validation_data = self.data.filter(pl.col("date") > split_date)

        return training_data, validation_data

    def _get_prediction_data(
        self,
        maximum_encoder_length: int = 35,
    ) -> pl.DataFrame:
        return (
            self.data.sort("timestamp")
            .group_by("ticker")
            .agg(pl.col("*").tail(maximum_encoder_length))
            .explode([col for col in self.data.columns if col != "ticker"])
        )

    def get_dimensions(self) -> dict[str, int]:
        return {
            "encoder_categorical_features": len(self.categorical_columns),
            "encoder_continuous_features": len(self.continuous_columns),
            "decoder_categorical_features": len(self.categorical_columns),
            "decoder_continuous_features": 0,  # not using decoder_continuous_features for now # noqa: E501
            "static_categorical_features": len(self.static_categorical_columns),
            "static_continuous_features": 0,  # not using static_continuous_features for now # noqa: E501
        }

    def get_batches(
        self,
        data_type: str = "train",  # "train", "validate", or "predict"
        validation_split: float = 0.8,
        input_length: int = 35,
        output_length: int = 7,
    ) -> list[dict[str, Tensor]]:
        batches = []

        if data_type not in {"train", "validate", "predict"}:
            message = f"Invalid data type: {data_type}. Must be 'train', 'validate', or 'predict'."  # noqa: E501
            raise ValueError(message)

        if data_type == "train":
            self.batch_data, _ = self._get_training_and_validation_data(
                validation_split
            )

        elif data_type == "validate":
            _, self.batch_data = self._get_training_and_validation_data(
                validation_split
            )

        elif data_type == "predict":
            self.batch_data = self._get_prediction_data(input_length + output_length)

        minimum_date: date = self.batch_data.select(
            self.batch_data["date"].min()
        ).item()
        maximum_date: date = self.batch_data.select(
            self.batch_data["date"].max()
        ).item()

        total_days = (maximum_date - minimum_date).days + 1
        required_days = input_length + output_length

        if total_days < required_days:
            message = (
                f"Total days available: {total_days}, required days: {required_days}."
            )
            raise ValueError(message)

        for ticker in self.batch_data["ticker"].unique():
            ticker_data = self.batch_data.filter(pl.col("ticker") == ticker).sort(
                "time_idx"
            )

            for i in range(len(ticker_data) - input_length - output_length + 1):
                encoder_slice = ticker_data[i : i + input_length]
                decoder_slice = ticker_data[
                    i + input_length : i + input_length + output_length
                ]

                # not using decoder_continuous_features (future-known numerical data
                # e.g. economic indicators, if available) or static_continuous_features
                # (constant numerical data per stock e.g. market cap, P/E ratio) for now
                batch = {
                    "encoder_categorical_features": Tensor(
                        encoder_slice[self.categorical_columns].to_numpy()
                    ),  # historical categorical data varying over time e.g. day of week, holiday status  # noqa: E501
                    "encoder_continuous_features": Tensor(
                        encoder_slice[self.continuous_columns].to_numpy()
                    ),  # historical numerical data varying over time e.g. price, volume
                    "decoder_categorical_features": Tensor(
                        decoder_slice[self.categorical_columns].to_numpy()
                    ),  # future-known categorical data e.g. future day of week, scheduled events  # noqa: E501
                    "static_categorical_features": Tensor(
                        ticker_data[self.static_categorical_columns].head(1).to_numpy()
                    ),  # constant categorical data per stock e.g. ticker, sector
                }

                if data_type in {"train", "validate"}:
                    batch["targets"] = Tensor(
                        decoder_slice[["daily_return"]].to_numpy()
                    )

                batches.append(batch)

        return batches

    def postprocess_predictions(
        self,
        input_batch: Tensor,  # static_categorical_features
        predictions: Tensor,  # quantiles
        current_datetime: datetime,
    ) -> pl.DataFrame:
        predictions_array = predictions.numpy()

        batch_size, output_length, _, _ = predictions_array.shape

        ticker_reverse_mapping = {v: k for k, v in self.mappings["ticker"].items()}

        rows = []
        for batch_idx in range(batch_size):
            ticker_encoded = int(
                input_batch["static_categorical_features"][batch_idx, 0, 0].item()
            )
            ticker_str = ticker_reverse_mapping[ticker_encoded]

            for time_idx in range(output_length):
                timestamp = int(
                    (current_datetime + timedelta(days=time_idx))
                    .replace(
                        hour=0,
                        minute=0,
                        second=0,
                        microsecond=0,
                    )
                    .timestamp()
                )

                quantile_values = predictions_array[batch_idx, time_idx, 0, :]

                daily_return_mean = self.scaler.means["daily_return"]
                daily_return_standard_deviation = self.scaler.standard_deviations[
                    "daily_return"
                ]

                unscaled_quantiles = (
                    quantile_values * daily_return_standard_deviation
                ) + daily_return_mean

                row = {
                    "ticker": ticker_str,
                    "timestamp": timestamp,
                    "quantile_10": unscaled_quantiles[0],
                    "quantile_50": unscaled_quantiles[1],
                    "quantile_90": unscaled_quantiles[2],
                }
                rows.append(row)

        return pl.DataFrame(rows)


dataset_schema = pa.DataFrameSchema(
    {
        "ticker": pa.Column(
            dtype=str,
            checks=pa.Check.str_matches(r"^[A-Z0-9.\-]+$"),
        ),
        "timestamp": pa.Column(
            dtype=int,
            checks=pa.Check.greater_than(0),
        ),
        "open_price": pa.Column(
            dtype=float,
            checks=pa.Check.greater_than(0),
        ),
        "high_price": pa.Column(
            dtype=float,
            checks=pa.Check.greater_than(0),
        ),
        "low_price": pa.Column(
            dtype=float,
            checks=pa.Check.greater_than(0),
        ),
        "close_price": pa.Column(
            dtype=float,
            checks=pa.Check.greater_than(0),
        ),
        "volume": pa.Column(
            dtype=int,
            checks=pa.Check.greater_than_or_equal_to(0),
        ),
        "volume_weighted_average_price": pa.Column(
            dtype=float,
            checks=pa.Check.greater_than_or_equal_to(0),
        ),
        "sector": pa.Column(dtype=str),
        "industry": pa.Column(dtype=str),
        "date": pa.Column(dtype=date),
        "day_of_week": pa.Column(dtype=int),
        "day_of_month": pa.Column(dtype=int),
        "day_of_year": pa.Column(dtype=int),
        "month": pa.Column(dtype=int),
        "year": pa.Column(dtype=int),
        "is_holiday": pa.Column(dtype=bool),
        "time_idx": pa.Column(dtype=int),
        "daily_return": pa.Column(dtype=float),
    },
    coerce=True,
)
