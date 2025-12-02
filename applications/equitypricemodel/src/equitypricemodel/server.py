import io
import os
from datetime import UTC, datetime, timedelta

import polars as pl
import requests
from fastapi import FastAPI
from internal.equity_bars_schema import equity_bars_schema
from pydantic import BaseModel

from .categories_schema import categories_schema
from .predictions_schema import predictions_schema
from .preprocess import filter_equity_bars
from .tide_data import Data
from .tide_model import Model

DATAMANAGER_BASE_URL = os.getenv("PSF_DATAMANAGER_BASE_URL", "http://datamanager:8080")


class PredictionRequest(BaseModel):
    pass


class PredictionResponse(BaseModel):
    data: dict


application = FastAPI()


@application.post("/predictions")
def create_predictions() -> PredictionResponse:  # TEMP
    tide_model = Model.load(directory_path=".")

    end_date = datetime.now(tz=UTC)
    start_date = end_date - timedelta(
        days=35
    )  # data preprocessing fills in more than 35 days

    equity_bars_response = requests.get(
        url=f"{DATAMANAGER_BASE_URL}/equity-bars",
        params={
            "start_date": start_date.isoformat(),
            "end_date": end_date.isoformat(),
        },
        timeout=60,
    )

    equity_details_response = requests.get(
        url=f"{DATAMANAGER_BASE_URL}/equity-details",
        timeout=60,
    )

    equity_bars_data = pl.read_parquet(io.BytesIO(equity_bars_response.content))

    equity_bars_data = equity_bars_schema.validate(equity_bars_data)

    equity_bars_data = filter_equity_bars(equity_bars_data)

    equity_categories_data = pl.read_json(equity_details_response.json())

    equity_categories_data = categories_schema.validate(equity_categories_data)

    consolidated_data = equity_categories_data.join(
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

    data = consolidated_data.select(retained_columns)

    current_timestamp = datetime.now(tz=UTC)

    tide_data = Data()

    tide_data.load(directory_path=".")

    tide_data.preprocess_and_set_data(data=data)

    batches = tide_data.get_batches(data_type="predict")

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

    save_predictions_response = requests.post(
        url=f"{DATAMANAGER_BASE_URL}/predictions",
        json={
            "timestamp": current_timestamp.isoformat(),
            "data": processed_predictions.to_dicts(),
        },
        timeout=60,
    )

    save_predictions_response.raise_for_status()

    return PredictionResponse(data=processed_predictions.to_dict())
