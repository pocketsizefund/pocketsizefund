import os
from datetime import UTC, datetime, timedelta

import polars as pl
import requests
from fastapi import FastAPI
from pydantic import BaseModel

from .tide_data import Data
from .tide_model import Model


class PredictionRequest(BaseModel):
    pass


class PredictionResponse(BaseModel):
    data: dict


application = FastAPI()


@application.post("/predictions")
def create_predictions() -> PredictionResponse:  # TEMP
    tide_model = Model.load(directory_path=".")

    datamanager_base_url = os.getenv("PSF_DATAMANAGER_BASE_URL", "")

    end_date = datetime.now(tz=UTC)
    start_date = end_date - timedelta(
        days=35
    )  # data preprocessing fills in more than 35 days

    equity_bars_response = requests.get(
        url=f"{datamanager_base_url}/equity-bars",
        params={
            "start_date": start_date.isoformat(),
            "end_date": end_date.isoformat(),
        },
        timeout=60,
    )

    equity_details_response = requests.get(
        url=f"{datamanager_base_url}/equity-details",
        timeout=60,
    )

    equity_bars_data = pl.read_json(equity_bars_response.json())
    equity_categories_data = pl.read_json(equity_details_response.json())

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

    tide_data = Data()

    tide_data.load(directory_path=".")

    tide_data.preprocess_and_set_data(data=data)

    batches = tide_data.get_batches(data_type="predict")

    predictions = tide_model.predict(
        inputs=batches[-1]
    )  # preprocessing generates more than 35 days

    return PredictionResponse(data={"predictions": predictions.numpy()})
