import polars as pl
import category_encoders as ce
from tinygrad import Tensor
from typing import Iterable, Dict, List, Any


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
        scalers: Dict[str, Dict[str, List[float]]] = None,  # TODO: change type
    ) -> None:
        self.batch_size = batch_size
        self.sample_count = sample_count
        self.scalers = scalers

    # TODO: change scaling to be grouped by ticker
    # TODO: add padding for all skipped days (e.g. weekends + holidays)
    # TODO: predict for 7 days (if padding is included)
    # TODO: add "day of week" feature
    # TODO: add "day of month" feature
    # TODO: add "month" feature
    # TODO: add "day of year" feature
    # TODO: add "year" feature
    def set_scalers(self, data: pl.DataFrame) -> None:
        self.scalers = {}  # TODO: add type
        for ticker, group in data.group_by("ticker"):
            means = group[continuous_variable_columns].mean()
            standard_deviations = group[continuous_variable_columns].std()

            self.scalers[ticker] = {"means": means, "standard_deviations": standard_deviations}

    def set_data(self, data: pl.DataFrame) -> None:
        if self.scalers is None:
            raise ValueError("Scalers attribute has not been set")

        for ticker, group in data.group_by("ticker"):
            means = self.scalers[ticker]["means"]
            standard_deviations = self.scalers[ticker]["standard_deviations"]

            data.loc[group.index, continuous_variable_columns] = (
                group[continuous_variable_columns] - means
            ) / standard_deviations

        ticker_series = data["ticker"].to_pandas()
        encoder = ce.OrdinalEncoder(
            cols=["ticker"],
            handle_unknown="use_encoded_value",
            handle_missing="use_encoded_value",
        )
        encoded_tickers = encoder.fit_transform(ticker_series)

        data = data.with_columns(pl.Series("ticker", encoded_tickers["ticker"]))

        data = data.with_columns(
            pl.col("timestamp").str.strptime(pl.Datetime, format="%Y-%m-%dT%H:%M:%SZ")
        )

        self.data = data

    def get_information(self) -> Dict[str, Any]:  # TODO: change type
        if self.scalers is None:
            raise ValueError("Scalers attribute has not been set")

        if self.data is None:
            raise ValueError("Data attribute has not been set")

        means_by_ticker = ({ticker: values["means"] for ticker, values in self.scalers.items()},)
        standard_deviations_by_ticker = (
            {ticker: values["standard_deviations"] for ticker, values in self.scalers.items()},
        )

        # TODO: convert to tensors (?)
        return {
            "means_by_ticker": means_by_ticker,
            "standard_deviations_by_ticker": standard_deviations_by_ticker,
            "encoder_lengths": [30] * len(self.data),
            "decoder_lengths": [5] * len(self.data),
            "encoder_categories": self.data[["ticker"]].values[:30],
            "decoder_categories": self.data[["ticker"]].values[-5:],
            "encoder_continous_variables": self.data[continuous_variable_columns].values[:30],
            "decoder_continous_variables": self.data[continuous_variable_columns].values[-7:],
        }

    def __iter__(self) -> Iterable[Tensor]:
        for i in range(0, self.sample_count, self.batch_size):
            batch = self.data[i : i + self.batch_size]
            if batch.shape[0] < self.batch_size:
                padding = Tensor.zeros((self.batch_size - batch.shape[0], *batch.shape[1:]))
                batch = Tensor(data=batch.to_numpy())
                batch = batch.stack(padding, dim=0)

            yield Tensor(data=batch.to_numpy())
