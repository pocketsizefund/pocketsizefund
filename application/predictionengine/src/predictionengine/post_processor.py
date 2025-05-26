from typing import Dict, Tuple, Any
from tinygrad.tensor import Tensor
from category_encoders import OrdinalEncoder
import numpy as np
import polars as pl


class PostProcessor:
    def __init__(
        self,
        means_by_ticker: Dict[str, Tensor],
        standard_deviations_by_ticker: Dict[str, Tensor],
        ticker_encoder: OrdinalEncoder,
    ) -> None:
        self.means_by_ticker = means_by_ticker
        self.standard_deviations_by_ticker = standard_deviations_by_ticker
        self.ticker_encoder = ticker_encoder

    def post_process_predictions(
        self,
        encoded_tickers: np.ndarray,
        predictions: np.ndarray,
    ) -> Tuple[
        np.ndarray[Any, np.dtype[np.float64]],
        np.ndarray[Any, np.dtype[np.float64]],
        np.ndarray[Any, np.dtype[np.float64]],
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
            mean = self.means_by_ticker[ticker].numpy()
            standard_deviation = self.standard_deviations_by_ticker[ticker].numpy()
            rescaled_predictions[i, :] = predictions[i, :] * standard_deviation + mean

        percentile_25 = np.percentile(rescaled_predictions, 25, axis=-1)
        percentile_50 = np.percentile(rescaled_predictions, 50, axis=-1)
        percentile_75 = np.percentile(rescaled_predictions, 75, axis=-1)

        return percentile_25, percentile_50, percentile_75
