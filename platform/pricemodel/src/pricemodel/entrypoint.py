"""Inference endpoint for price prediction model."""

import datetime
import os

from fastapi import FastAPI, status
from fastapi_cloudevents import CloudEvent, install_fastapi_cloudevents
from loguru import logger
from pocketsizefund import config, data, model
from pydantic import BaseModel
from sentry_sdk.integrations.loguru import LoggingLevels, LoguruIntegration

ENVIRONMENT = os.getenv("ENVIRONMENT")
DATA_PROVIDER_URL = f"http://data-provider.{ENVIRONMENT}.svc.cluster.local:8080"


sentry_loguru = LoguruIntegration(
    level=LoggingLevels.INFO.value,
    event_level=LoggingLevels.ERROR.value,
)

sentry_sdk.init(
    dsn=os.getenv("SENTRY_DSN"),
    integrations=[sentry_loguru],
    traces_sample_rate=1.0,
)


from pricemodel.trade import Client

FUND = os.getenv("FUND")

trade_client = Client(
    darqube_api_key=os.getenv("DARQUBE_API_KEY"),
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
    is_paper=FUND == "paper",
)


price_model = model.PriceModel()

try:
    price_model.load_model(file_path="price-model.ckpt")
    logger.info(f"loaded {price_model=}")
except FileNotFoundError:
    logger.exception("model not found, make sure MODEL_FILE_NAME is set")
    price_model = None
except IsADirectoryError:
    logger.exception("model is a directory, make sure MODEL_FILE_NAME is set")
    price_model = None

app = FastAPI()
app = install_fastapi_cloudevents(app)


@app.get("/health", status_code=status.HTTP_200_OK)
def health() -> CloudEvent:
    """Health check endpoint that the cluster pings to ensure the service is up."""
    return CloudEvent(
        type="health.check",
        source="psf.platform.predictionmodel",
        data=None,
    )


Ticker = dict[str, list[float]]


class Predictions(BaseModel):
    tickers: Ticker


@app.post("/")
async def invocations(event: CloudEvent) -> CloudEvent:
    """Invocations handles prediction requests to the inference endpoint."""
    logger.info(f"received event: {event}")
    if price_model is None:
        return CloudEvent(
            type="prediction.error",
            source="psf.platform.predictionmodel",
            data={"error": "model not found, make sure MODEL_FILE_NAME is set"},
        )

    available_tickers = trade_client.get_available_tickers()

    end_at = datetime.datetime.now(tz=config.TIMEZONE)
    start_at = end_at - datetime.timedelta(days=20)

    response = requests.post(
        url=DATA_PROVIDER_URL + "/",
        timeout=30,
        json={
            "data": {
                "start_at": start_at,
                "end_at": end_at,
            },
        },
    )

    response = requests.post(DATA_PROVIDER_URL)

    if response.status_code != status.HTTP_200_OK:
        return Response(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            content="error getting data",
            media_type="text/plain",
        )

    json_data = response.json()

    equity_bars_raw_data = pd.DataFrame(json_data)

    filtered_equity_bars_raw_data = equity_bars_raw_data["ticker"].isin(
        available_tickers
    )

    equity_bars_raw_data_grouped_by_ticker = filtered_equity_bars_raw_data.groupby(
        "ticker"
    )

    predictions: dict[str, list[float]] = {}
    for ticker, ticker_bars_raw_data in equity_bars_raw_data_grouped_by_ticker:
        ticker_predictions: list[float] = price_model.get_predictions(
            data=ticker_bars_raw_data,
        )

        predictions[ticker] = ticker_predictions

    return CloudEvent(
        type="psf.platform.predictionmodel",
        source="prediction.success",
        data={"tickers": predictions},
    )
