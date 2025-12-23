import io
import os
from datetime import UTC, datetime, timedelta

import polars as pl
import requests
import structlog
from fastapi import FastAPI, Response, status
from internal.equity_bars_schema import equity_bars_schema

from .equity_details_schema import equity_details_schema
from .predictions_schema import predictions_schema
from .preprocess import filter_equity_bars
from .tide_data import Data
from .tide_model import Model

logger = structlog.get_logger()

DATAMANAGER_BASE_URL = os.getenv("PSF_DATAMANAGER_BASE_URL", "http://datamanager:8080")


application = FastAPI()


tide_model = Model.load(directory_path=".")


@application.post("/predictions")
def create_predictions() -> Response:
    logger.info("Starting prediction generation process")

    end_date = datetime.now(tz=UTC)
    start_date = end_date - timedelta(
        days=35
    )  # data preprocessing fills in more than 35 days

    try:
        equity_bars_response = requests.get(
            url=f"{DATAMANAGER_BASE_URL}/equity-bars",
            params={
                "start_date": start_date.isoformat(),
                "end_date": end_date.isoformat(),
            },
            timeout=60,
        )

        equity_bars_response.raise_for_status()

    except Exception as e:
        logger.exception(
            "Failed to fetch equity bars data",
            start_date=start_date.isoformat(),
            end_date=end_date.isoformat(),
            error=f"{e}",
        )

        return Response(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR)

    try:
        equity_details_response = requests.get(
            url=f"{DATAMANAGER_BASE_URL}/equity-details",
            timeout=60,
        )

        equity_details_response.raise_for_status()

    except Exception as e:
        logger.exception(
            "Failed to fetch equity details data",
            error=f"{e}",
        )

        return Response(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR)

    try:
        data = parse_responses(
            equity_bars_response=equity_bars_response,
            equity_details_response=equity_details_response,
        )
    except Exception as e:
        logger.exception(
            "Failed to parse and consolidate data responses",
            error=f"{e}",
        )
        return Response(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR)

    current_timestamp = datetime.now(tz=UTC)

    tide_data = Data.load(directory_path=".")

    tide_data.preprocess_and_set_data(data=data)

    batches = tide_data.get_batches(data_type="predict")

    if not batches:
        logger.error("No data batches available for prediction")
        return Response(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR)

    raw_predictions = tide_model.predict(
        inputs=batches[-1]
    )  # preprocessing generates more than 35 days

    predictions = tide_data.postprocess_predictions(
        input_batch=batches[-1],
        predictions=raw_predictions,
        current_datetime=current_timestamp,
    )

    # filter to only the 7th timestep
    processed_prediction_timestamp = current_timestamp + timedelta(days=6)
    processed_predictions = predictions.filter(
        pl.col("timestamp")
        == int(
            processed_prediction_timestamp.replace(
                hour=0, minute=0, second=0, microsecond=0
            ).timestamp()
        )
    )

    processed_predictions = predictions_schema.validate(processed_predictions)

    try:
        save_predictions_response = requests.post(
            url=f"{DATAMANAGER_BASE_URL}/predictions",
            json={
                "timestamp": current_timestamp.isoformat(),
                "data": processed_predictions.to_dicts(),
            },
            timeout=60,
        )

        save_predictions_response.raise_for_status()

    except Exception as e:
        logger.exception(
            "Failed to save predictions data",
            timestamp=current_timestamp.isoformat(),
            error=f"{e}",
        )
        raise

    logger.info("Successfully generated and saved predictions")

    return Response(
        content={"data": processed_predictions.to_dict()},
        status_code=status.HTTP_200_OK,
    )


def parse_responses(
    equity_bars_response: requests.Response,
    equity_details_response: requests.Response,
) -> pl.DataFrame:
    equity_bars_data = pl.read_parquet(io.BytesIO(equity_bars_response.content))

    equity_bars_data = equity_bars_schema.validate(equity_bars_data)

    equity_bars_data = filter_equity_bars(equity_bars_data)

    equity_details_data = pl.DataFrame(equity_details_response.json())

    equity_details_data = equity_details_schema.validate(equity_details_data)

    consolidated_data = equity_details_data.join(
        equity_bars_data, on="ticker", how="inner"
    )

    retained_columns = (
        "ticker",
        "timestamp",
        "open_price",
        "high_price",
        "low_price",
        "close_price",
        "volume",
        "volume_weighted_average_price",
        "sector",
        "industry",
    )

    return consolidated_data.select(retained_columns)
