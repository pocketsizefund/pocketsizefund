import polars as pl
import category_encoders as ce
from tinygrad import Tensor
from typing import Iterable, Dict, Union


class DataSet:
    def __init__(
        self,
        batch_size: int,
        sample_count: int,
        means: int = None,
        standard_deviations: int = None,
    ) -> None:
        self.batch_size = batch_size
        self.sample_count = sample_count
        self.means = means
        self.standard_deviations = standard_deviations

    def set_scalers(self, data: pl.DataFrame) -> None:
        self.skipped_columns = ["ticker", "timestamp"]

        filtered_data = data.drop(self.skipped_columns)

        self.means = filtered_data.select(
            [pl.col(c).mean().alias(c) for c in filtered_data.columns]
        )

        self.standard_deviations = filtered_data.select(
            [pl.col(c).std().alias(c) for c in filtered_data.columns]
        )

    def set_data(self, data: pl.DataFrame) -> None:
        if self.means is None or self.standard_deviations is None:
            raise ValueError("Scaler parameters have not been set")

        filtered_data = data.drop(self.skipped_columns)

        textual_data = data.select(self.skipped_columns)

        ticker_series = textual_data["ticker"].to_pandas()
        encoder = ce.OrdinalEncoder(
            cols=["ticker"],
            handle_unknown="use_encoded_value",
            handle_missing="use_encoded_value",
        )
        encoded_tickers = encoder.fit_transform(ticker_series)

        textual_data = textual_data.with_columns(pl.Series("ticker", encoded_tickers["ticker"]))

        textual_data = textual_data.with_columns(
            pl.col("timestamp").str.strptime(pl.Datetime, format="%Y-%m-%dT%H:%M:%SZ")
        )

        textual_data = textual_data.with_columns(
            pl.col("timestamp").dt.timestamp().alias("timestamp")
        )

        temporal_data = filtered_data.select(
            [
                (pl.col(c) - self.means[c]) / self.standard_deviations[c]
                for c in filtered_data.columns
            ]
        )

        normalized_data = pl.concat([textual_data, temporal_data], how="horizontal")

        self.data = normalized_data

    def get_information(self) -> Dict[str, Union[str, int]]:
        return {
            "means": self.means,  # NOTE: make Tensor (?)
            "standard_deviations": self.standard_deviations,  # NOTE: make Tensor (?)
            # "encoder_lengths": [],  # TEMP
            # "decoder_lengths": [],  # TEMP
            # "encoder_categories": Tensor,
            # "decoder_categories": Tensor,
        }

    def __iter__(self) -> Iterable[Tensor]:
        for i in range(0, self.sample_count, self.batch_size):
            batch = self.data[i : i + self.batch_size]
            if batch.shape[0] < self.batch_size:
                padding = Tensor.zeros((self.batch_size - batch.shape[0], *batch.shape[1:]))
                batch = Tensor(data=batch.to_numpy())
                batch = batch.stack(padding, dim=0)

            yield Tensor(data=batch.to_numpy())
