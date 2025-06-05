import os
import traceback
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from datetime import UTC, date, datetime, timedelta
from pathlib import Path

import polars as pl
import requests
from fastapi import FastAPI, HTTPException, Request, Response, status
from loguru import logger
from prometheus_fastapi_instrumentator import Instrumentator

from .dataset import DataSet
from .miniature_temporal_fusion_transformer import MiniatureTemporalFusionTransformer
from .models import PredictionResponse

SEQUENCE_LENGTH = 30


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None]:
    datamanager_base_url = os.getenv("DATAMANAGER_BASE_URL", "")
    app.state.datamanager_base_url = datamanager_base_url

    app.state.model = None
    yield


application = FastAPI(lifespan=lifespan)
Instrumentator().instrument(application).expose(application)


@application.get("/health")
async def health_check() -> Response:
    return Response(status_code=status.HTTP_200_OK)


def fetch_historical_data(
    datamanager_url: str, start_date: date, end_date: date
) -> pl.DataFrame:
    url = f"{datamanager_url}/equity-bars"
    parameters = {
        "start_date": start_date.isoformat(),
        "end_date": end_date.isoformat(),
    }

    response = requests.get(url, params=parameters, timeout=SEQUENCE_LENGTH)
    response.raise_for_status()

    import pyarrow as pa

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
            logger.warning(f"Failed to load model weights: {e}")

    return model


@application.post("/create-predictions")
async def create_predictions(
    request: Request,
) -> PredictionResponse:
    try:
        end_date = datetime.now(tz=UTC).date()
        start_date = end_date - timedelta(days=SEQUENCE_LENGTH)

        logger.info(f"Fetching data from {start_date} to {end_date}")
        data = fetch_historical_data(
            request.app.state.datamanager_base_url, start_date, end_date
        )

        if data.is_empty():
            raise HTTPException(  # noqa: TRY301
                status_code=404, detail="No data available for prediction"
            )

        if request.app.state.model is None:
            logger.info("Initializing model")
            request.app.state.model = load_or_initialize_model(data)

        model = request.app.state.model

        unique_tickers = data["ticker"].unique().to_list()
        predictions = {}

        for ticker in unique_tickers:
            ticker_data = data.filter(pl.col("ticker") == ticker)
            if len(ticker_data) < SEQUENCE_LENGTH:
                logger.warning(f"Insufficient data for ticker {ticker}")
                continue

            recent_data = ticker_data.tail(SEQUENCE_LENGTH)

            dataset = DataSet(
                batch_size=1,
                sequence_length=SEQUENCE_LENGTH,
                sample_count=1,
            )
            dataset.load_data(recent_data)

            try:
                tickers_batch, features_batch, _ = next(iter(dataset.batches()))
            except StopIteration:
                logger.warning(f"No batches available for ticker {ticker}")
                continue

            percentile_25, percentile_50, percentile_75 = model.predict(
                tickers_batch, features_batch
            )

            predictions[ticker] = {
                "percentile_25": float(percentile_25[0]),
                "percentile_50": float(percentile_50[0]),
                "percentile_75": float(percentile_75[0]),
            }

        if not predictions:
            raise HTTPException(  # noqa: TRY301
                status_code=404, detail="No predictions could be generated"
            )

        return PredictionResponse(predictions=predictions)

    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error creating predictions: {e}")
        logger.error(traceback.format_exc())
        raise HTTPException(
            status_code=500, detail=f"Internal server error: {e!s}"
        ) from e
