from typing import Dict, Tuple
from tinygrad import Tensor
from category_encoders import OrdinalEncoder
import numpy as np
import polars as pl  # NOTE: remove


class PostProcessor:
    def __init__(
        self,
        means_by_ticker: Tuple[Dict[str, Tensor]],
        standard_deviations_by_ticker: Tuple[Dict[str, Tensor]],
        ticker_encoder: OrdinalEncoder,
    ) -> None:
        self.means_by_ticker = means_by_ticker
        self.standard_deviations_by_ticker = standard_deviations_by_ticker
        self.ticker_encoder = ticker_encoder

    def process_predictions(
        self,
        tickers: Tensor,
        predictions: Tensor,
    ) -> Tensor:
        predictions = predictions.realize()  # TEMP (remove)

        print("postprocessor - predictions.shape:", predictions.shape)  # TEMP

        batch_size, sequence_length, quantiles = predictions.shape

        print(
            "postprocessor - batch_size:", batch_size, "sequence_length:", sequence_length
        )  # TEMP

        means = Tensor.stack(
            *[self.means_by_ticker[int(t)] for t in tickers.tolist()],
            dim=0,
        ).realize()  # (30, 6) NOTE: rename (keep realize)

        stds = Tensor.stack(
            *[self.standard_deviations_by_ticker[int(t)] for t in tickers.tolist()],
            dim=0,
        ).realize()  # NOTE: rename (keeep realize)

        print("postprocessor - means.shape:", means.shape, "stds.shape:", stds.shape)  # TEMP

        close_idx = 3  # NOTE: confirm this or pass it in somehow

        # NOTE: rename all of these variables
        means_close = (
            means[:, close_idx].reshape(batch_size, 1, 1).realize()  # remove realize
        )  # Broadcastable to (30, 5, 3)
        stds_close = (
            stds[:, close_idx].reshape(batch_size, 1, 1).realize()
        )  # NOTE: rename (remove realize)
        rescaled = predictions * stds_close + means_close  # (30, 5, 3) NOTE: rename

        print(
            f"Post-processor rescaled shape: {rescaled.shape}, requires_grad: {rescaled.requires_grad}"
        )  # TEMP

        print("postprocessor - rescaled.shape:", rescaled.shape)  # TEMP

        rescaled = rescaled.realize()

        return rescaled
