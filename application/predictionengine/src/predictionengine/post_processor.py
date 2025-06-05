import numpy as np
import numpy.typing as npt
import polars as pl
from category_encoders import OrdinalEncoder
from tinygrad.tensor import Tensor

TensorMapping = dict[str, Tensor]


class PostProcessor:
    def __init__(
        self,
        means_by_ticker: TensorMapping,
        standard_deviations_by_ticker: TensorMapping,
        ticker_encoder: OrdinalEncoder,
    ) -> None:
        self.means_by_ticker: TensorMapping = means_by_ticker
        self.standard_deviations_by_ticker: TensorMapping = (
            standard_deviations_by_ticker
        )
        self.ticker_encoder: OrdinalEncoder = ticker_encoder

    def post_process_predictions(
        self,
        encoded_tickers: npt.NDArray[np.float64],
        predictions: npt.NDArray[np.float64],
    ) -> tuple[
        npt.NDArray[np.float64],
        npt.NDArray[np.float64],
        npt.NDArray[np.float64],
    ]:
        decoded_tickers = self.ticker_encoder.inverse_transform(
            pl.DataFrame(
                {
                    "ticker": encoded_tickers,
                }
            ).to_pandas()
        )["ticker"]

        rescaled_predictions = np.empty_like(predictions)

        for i, ticker in enumerate(decoded_tickers):
            if (
                ticker not in self.means_by_ticker
                or ticker not in self.standard_deviations_by_ticker
            ):
                msg = f"Statistics not found for ticker: {ticker}"
                raise ValueError(msg)

            mean = self.means_by_ticker[ticker].numpy()
            standard_deviation = self.standard_deviations_by_ticker[ticker].numpy()
            rescaled_predictions[i, :] = predictions[i, :] * standard_deviation + mean
        percentile_25 = np.percentile(rescaled_predictions, 25, axis=-1)
        percentile_50 = np.percentile(rescaled_predictions, 50, axis=-1)
        percentile_75 = np.percentile(rescaled_predictions, 75, axis=-1)

        return percentile_25, percentile_50, percentile_75
