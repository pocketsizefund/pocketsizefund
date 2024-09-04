"""Set positions based on portfolio position and model predictions."""

import os
import random

import requests
from fastapi import FastAPI, status
from loguru import logger
from pocketsizefund import trade

POSITIONS_COUNT = 10

PRICE_MODEL_URL = "http://price-model.default.svc.cluster.local:8080"

app = FastAPI()


@app.get("/health", status_code=status.HTTP_200_OK)
def health() -> None:
    """Health check endpoint that the cluster pings to ensure the service is up."""
    return


@app.post("/")
def predictions() -> None:
    """Set positions based on portfolio position and model predictions."""
    trade_client = trade.Client(
        darqube_api_key=os.getenv("DARQUBE_API_KEY"),
        alpaca_api_key=os.getenv("ALPACA_API_KEY"),
        alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
        alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
        is_paper=os.getenv("IS_PAPER"),
    )

    response = requests.get(
        url=PRICE_MODEL_URL + "/predictions",
        timeout=30,
    )

    response.raise_for_status()

    predictions_by_ticker = response.json()

    logger.info(f"predictions_by_ticker: {predictions_by_ticker}")

    random_ticker = random.choice(list(predictions_by_ticker.get("tickers").keys()))  # noqa: S311

    logger.info(f"random_ticker: {random_ticker}")

    trade_client.baseline_buy(ticker=random_ticker)
