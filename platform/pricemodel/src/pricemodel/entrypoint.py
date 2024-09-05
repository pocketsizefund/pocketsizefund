"""Inference endpoint for price prediction model."""

import datetime
import os

from fastapi import FastAPI, Response, status
from loguru import logger
from pocketsizefund import config, data, model
from pocketsizefund.trade import Client
from pydantic import BaseModel

trade_client = Client(
    darqube_api_key=os.getenv("DARQUBE_API_KEY"),
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
    alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
    is_paper=True,
)

data_client = data.Client(
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
    edgar_user_agent=os.getenv("EDGAR_USER_AGENT"),
    debug=False,
)

price_model = model.PriceModel()

try:
    price_model.load_model(
        file_path="price-model.ckpt"
    )
    logger.info(f"loaded {price_model=}")
except FileNotFoundError:
    logger.exception("model not found, make sure MODEL_FILE_NAME is set")
    price_model = None

except IsADirectoryError:
    logger.exception("model is a directory, make sure MODEL_FILE_NAME is set")
    price_model = None


app = FastAPI()


@app.get("/health", status_code=status.HTTP_200_OK)
def health() -> None:
    """Health check endpoint that the cluster pings to ensure the service is up."""
    return


Ticker = dict[str, list[float]]


class Predictions(BaseModel):
    tickers: Ticker


@app.get("/predictions")
def invocations() -> Predictions:
    """Invocations handles prediction requests to the inference endpoint."""
    if price_model is None:
        return Response(
            status_code=status.HTTP_404_NOT_FOUND,
            content="model not found, make sure MODEL_FILE_NAME is set",
            media_type="text/plain",
        )

    available_tickers = trade_client.get_available_tickers()

    end_at = datetime.datetime.now(tz=config.TIMEZONE)
    start_at = end_at - datetime.timedelta(days=20)

    equity_bars_raw_data = data_client.get_range_equities_bars(
        tickers=available_tickers,
        start_at=start_at,
        end_at=end_at,
    )

    equity_bars_raw_data_grouped_by_ticker = equity_bars_raw_data.groupby("ticker")

    predictions: dict[str, list[float]] = {}
    for ticker, ticker_bars_raw_data in equity_bars_raw_data_grouped_by_ticker:
        ticker_predictions: list[float] = price_model.get_predictions(
            data=ticker_bars_raw_data,
        )

        predictions[ticker] = ticker_predictions

    return {"tickers": predictions}
