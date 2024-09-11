"""Set positions based on portfolio position and model predictions."""

import os
import random

import requests
from fastapi import FastAPI
from loguru import logger
from pocketsizefund.trade import trade

POSITIONS_COUNT = 10

ENVIRONMENT = os.getenv("ENVIRONMENT")
PRICE_MODEL_URL = f"http://price-model.{ENVIRONMENT}.svc.cluster.local:8080"

app = FastAPI()

@app.on_event("startup")
def startup() -> None:
    """Startup event handler."""
    logger.info(f"price model url: {PRICE_MODEL_URL}")
    response = requests.get(
        url=PRICE_MODEL_URL + "/health",
        timeout=30,
    )
    logger.info(f"price-model response: status={response.status_code}: {response.text}")

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
