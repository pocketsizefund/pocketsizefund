from category_encoders import OrdinalEncoder
import polars as pl
from tinygrad import Tensor
from tinygrad.dtype import dtypes
import numpy as np
from pricemodel.post_processor import PostProcessor


def test_post_processor():
    tickers = ["AAPL", "GOOGL", "MSFT"]

    ticker_encoder = OrdinalEncoder(cols=["ticker"])
    ticker_encoder.fit(pl.DataFrame({"ticker": tickers}).to_pandas())

    means_by_ticker = {
        "AAPL": Tensor(np.array(150.0), dtype=dtypes.float32),
        "GOOGL": Tensor(np.array(2800.0), dtype=dtypes.float32),
        "MSFT": Tensor(np.array(300.0), dtype=dtypes.float32),
    }

    standard_deviations_by_ticker = {
        "AAPL": Tensor(np.array(5.0), dtype=dtypes.float32),
        "GOOGL": Tensor(np.array(50.0), dtype=dtypes.float32),
        "MSFT": Tensor(np.array(10.0), dtype=dtypes.float32),
    }

    encoded_tickers = ticker_encoder.transform(
        pl.DataFrame(
            {
                "ticker": tickers,
            }
        ).to_pandas()
    )["ticker"].to_numpy()

    predictions = np.array(
        [
            [0.2, -0.5, 0.1, 0.3, -0.2],  # AAPL
            [1.0, 0.7, -0.2, 0.5, 0.1],  # GOOGL
            [-0.3, 0.8, 0.6, -0.1, -0.4],  # MSFT
        ]
    )

    post_processor = PostProcessor(
        means_by_ticker=means_by_ticker,
        standard_deviations_by_ticker=standard_deviations_by_ticker,
        ticker_encoder=ticker_encoder,
    )

    percentile_25, percentile_50, percentile_75 = post_processor.post_process_predictions(
        encoded_tickers=encoded_tickers,
        predictions=predictions,
    )

    assert np.allclose(percentile_25, [149.0, 2805.0, 297.0])
    assert np.allclose(percentile_50, [150.5, 2825.0, 299.0])
    assert np.allclose(percentile_75, [151.0, 2835.0, 306.0])
