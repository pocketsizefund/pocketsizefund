import os
import traceback
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from datetime import date, datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

import polars as pl
import pyarrow as pa
import requests
from cloudevents.pydantic.v2 import CloudEvent
from fastapi import FastAPI, Request, Response, status
from loguru import logger
from prometheus_fastapi_instrumentator import Instrumentator

from .dataset import DataSet
from .miniature_temporal_fusion_transformer import MiniatureTemporalFusionTransformer

SEQUENCE_LENGTH = 30


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None, None]:
    app.state.datamanager_base_url = os.getenv("DATAMANAGER_BASE_URL", "")
    app.state.model = None

    yield

    if hasattr(app.state, "model"):
        app.state.model = None


application = FastAPI(lifespan=lifespan)
Instrumentator().instrument(application).expose(application)


@application.get("/health")
def get_health() -> Response:
    return Response(status_code=status.HTTP_200_OK)


def fetch_historical_data(
    datamanager_url: str,
    start_date: date,
    end_date: date,
) -> pl.DataFrame:
    url = f"{datamanager_url}/equity-bars"
    parameters = {
        "start_date": start_date.isoformat(),
        "end_date": end_date.isoformat(),
    }

    response = requests.get(
        url=url,
        params=parameters,
        timeout=120,
    )
    response.raise_for_status()

    buffer = pa.py_buffer(response.content)
    reader = pa.ipc.RecordBatchStreamReader(buffer)
    table = reader.read_all()

    return pl.DataFrame(pl.from_arrow(table))


def load_or_initialize_model(data: pl.DataFrame) -> MiniatureTemporalFusionTransformer:
    dataset = DataSet(
        batch_size=32,
        sequence_length=SEQUENCE_LENGTH,
        sample_count=len(data),
    )
    dataset.load_data(data)
    preprocessors = dataset.get_preprocessors()

    model = MiniatureTemporalFusionTransformer(
        input_size=6,
        hidden_size=128,
        output_size=3,
        layer_count=2,
        ticker_count=len(data["ticker"].unique()),
        embedding_size=32,
        attention_head_count=4,
        means_by_ticker=preprocessors["means_by_ticker"],
        standard_deviations_by_ticker=preprocessors["standard_deviations_by_ticker"],
        ticker_encoder=preprocessors["ticker_encoder"],
        dropout_rate=0.0,
    )
    model_path = "miniature_temporal_fusion_transformer.safetensor"
    if Path(model_path).exists():
        try:
            model.load(model_path)
            logger.info("Loaded existing model weights")
        except Exception as e:  # noqa: BLE001
            logger.warning(f"Failed to load model weights: {e}")

    return model


def get_predictions(
    tickers: list[str],
    data: pl.DataFrame,
    model: MiniatureTemporalFusionTransformer,
) -> dict[str, dict[str, list[float]]]:
    predictions: dict[str, dict[str, list[float]]] = {}

    valid_tickers = []
    for ticker in tickers:
        ticker_data = data.filter(pl.col("ticker") == ticker)
        if len(ticker_data) < SEQUENCE_LENGTH:
            logger.warning(f"Insufficient data for ticker: {ticker}")
            continue
        valid_tickers.append(ticker)

    if not valid_tickers:
        return predictions

    filtered_data = data.filter(pl.col("ticker").is_in(valid_tickers))
    dataset = DataSet(
        batch_size=len(valid_tickers),
        sequence_length=SEQUENCE_LENGTH,
        sample_count=len(valid_tickers),
    )
    dataset.load_data(filtered_data)

    try:
        tickers_batch, features_batch, _ = next(iter(dataset.batches()))
    except StopIteration:
        logger.warning("No batches available for prediction")
        return predictions

    percentile_25, percentile_50, percentile_75 = model.predict(
        tickers_batch,
        features_batch,
    )

    for i, ticker in enumerate(valid_tickers):
        predictions[ticker] = {
            "percentile_25": percentile_25[i].tolist(),
            "percentile_50": percentile_50[i].tolist(),
            "percentile_75": percentile_75[i].tolist(),
        }

    return predictions


@application.post("/predictions/create")
def create_predictions(request: Request) -> CloudEvent:
    try:
        end_date = datetime.now(tz=ZoneInfo("America/New_York")).date()
        start_date = end_date - timedelta(days=SEQUENCE_LENGTH)

        logger.info(f"Fetching data start and end dates: {start_date}, {end_date}")
        data = fetch_historical_data(
            request.app.state.datamanager_base_url, start_date, end_date
        )

        if data.is_empty():
            return CloudEvent(
                attributes={
                    "source": "predictionengine",
                    "type": "application.predictionengine.predictions.errored",
                },
                data={
                    "date": end_date.isoformat(),
                    "message": "No data available for prediction",
                },
            )

        if request.app.state.model is None:
            logger.info("Initializing model")
            request.app.state.model = load_or_initialize_model(data)

        model = request.app.state.model

        unique_tickers = data["ticker"].unique().to_list()

        predictions = get_predictions(
            tickers=unique_tickers,
            data=data,
            model=model,
        )

        if not predictions:
            return CloudEvent(
                attributes={
                    "source": "predictionengine",
                    "type": "application.predictionengine.predictions.errored",
                },
                data={
                    "date": end_date.isoformat(),
                    "message": "No predictions could be generated",
                },
            )

        return CloudEvent(
            attributes={
                "source": "predictionengine",
                "type": "application.predictionengine.predictions.created",
            },
            data={
                "date": end_date.isoformat(),
                "predictions": predictions,
            },
        )

    except Exception as e:  # noqa: BLE001
        logger.error(f"Error creating predictions: {e}")
        logger.error(traceback.format_exc())

        return CloudEvent(
            attributes={
                "source": "predictionengine",
                "type": "application.predictionengine.predictions.errored",
            },
            data={
                "date": datetime.now(tz=ZoneInfo("America/New_York"))
                .date()
                .isoformat(),
                "error": str(e),
            },
        )
