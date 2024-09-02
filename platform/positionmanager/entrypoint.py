"""Set positions based on portfolio position and model predictions."""

import os
import random

from fastapi import FastAPI
from loguru import logger
from pocketsizefund.trade import trade

POSITIONS_COUNT = 10

ENVIRONMENT = os.getenv("ENVIRONMENT")

app = FastAPI()

@app.post("/")
def predictions() -> None:
    """Set positions based on portfolio position and model predictions."""
    # TODO: fill in with cloud event

    trade_client = trade.Client(
        darqube_api_key=os.getenv("DARQUBE_API_KEY"),
        alpaca_api_key=os.getenv("ALPACA_API_KEY"),
        alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
        alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
        is_paper=os.getenv("IS_PAPER"),
    )


    # logger.info(f"predictions_by_ticker: {predictions_by_ticker}")
    #
    # random_ticker = random.choice(list(predictions_by_ticker.get("tickers").keys()))  # noqa: S311
    #
    # logger.info(f"random_ticker: {random_ticker}")
    #
    # trade_client.baseline_buy(ticker=random_ticker)
