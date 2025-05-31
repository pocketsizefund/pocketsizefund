from category_encoders import OrdinalEncoder
import polars as pl
from tinygrad.tensor import Tensor
import numpy as np
from application.predictionengine.src.predictionengine.post_processor import (
    PostProcessor,
)


def test_post_processor_initialization() -> None:
    ticker_encoder = OrdinalEncoder(cols=["ticker"])
    means_by_ticker = {"AAPL": Tensor([150.0])}
    standard_deviations_by_ticker = {"AAPL": Tensor([5.0])}

    post_processor = PostProcessor(
        means_by_ticker=means_by_ticker,
        standard_deviations_by_ticker=standard_deviations_by_ticker,
        ticker_encoder=ticker_encoder,
    )

    assert post_processor.means_by_ticker == means_by_ticker
    assert post_processor.standard_deviations_by_ticker == standard_deviations_by_ticker
    assert post_processor.ticker_encoder == ticker_encoder


def test_post_processor_predictions() -> None:
    tickers = ["AAPL", "GOOGL"]

    ticker_encoder = OrdinalEncoder(cols=["ticker"])
    ticker_encoder.fit(pl.DataFrame({"ticker": tickers}).to_pandas())

    means_by_ticker = {
        "AAPL": Tensor([150.0]),
        "GOOGL": Tensor([2800.0]),
    }

    standard_deviations_by_ticker = {
        "AAPL": Tensor([5.0]),
        "GOOGL": Tensor([50.0]),
    }

    encoded_tickers = ticker_encoder.transform(
        pl.DataFrame({"ticker": tickers}).to_pandas()
    )["ticker"].to_numpy()

    predictions = np.array(
        [
            [0.0, 1.0, -1.0],  # AAPL: mean, +1std, -1std
            [0.0, 1.0, -1.0],  # GOOGL: mean, +1std, -1std
        ]
    )

    post_processor = PostProcessor(
        means_by_ticker=means_by_ticker,
        standard_deviations_by_ticker=standard_deviations_by_ticker,
        ticker_encoder=ticker_encoder,
    )

    percentile_25, percentile_50, percentile_75 = (
        post_processor.post_process_predictions(
            encoded_tickers=encoded_tickers,
            predictions=predictions,
        )
    )

    assert isinstance(percentile_25, np.ndarray)
    assert isinstance(percentile_50, np.ndarray)
    assert isinstance(percentile_75, np.ndarray)
    assert len(percentile_25) == 2
    assert len(percentile_50) == 2
    assert len(percentile_75) == 2

    assert np.all(percentile_25 <= percentile_50)
    assert np.all(percentile_50 <= percentile_75)


def test_post_processor_single_ticker() -> None:
    ticker_encoder = OrdinalEncoder(cols=["ticker"])
    ticker_encoder.fit(pl.DataFrame({"ticker": ["AAPL"]}).to_pandas())

    means_by_ticker = {"AAPL": Tensor([100.0])}
    standard_deviations_by_ticker = {"AAPL": Tensor([10.0])}

    encoded_tickers = ticker_encoder.transform(
        pl.DataFrame({"ticker": ["AAPL"]}).to_pandas()
    )["ticker"].to_numpy()
    predictions = np.array([[0.5, 1.0, 1.5]])  # single prediction

    post_processor = PostProcessor(
        means_by_ticker=means_by_ticker,
        standard_deviations_by_ticker=standard_deviations_by_ticker,
        ticker_encoder=ticker_encoder,
    )

    percentile_25, percentile_50, percentile_75 = (
        post_processor.post_process_predictions(
            encoded_tickers=encoded_tickers,
            predictions=predictions,
        )
    )

    assert len(percentile_25) == 1
    assert len(percentile_50) == 1
    assert len(percentile_75) == 1
