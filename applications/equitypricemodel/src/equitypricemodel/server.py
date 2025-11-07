import os

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

    equity_bars_response = requests.get(
        url=os.getenv("PSF_DATAMANAGER_EQUITY_BARS_URL", ""),
        timeout=60,
    )

    equity_categories_response = requests.get(
        url=os.getenv("PSF_DATAMANAGER_EQUITY_CATEGORIES_URL", ""),
        timeout=60,
    )

    equity_bars_data = pl.read_json(equity_bars_response.json())
    equity_categories_data = pl.read_json(equity_categories_response.json())

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

    predictions = tide_model.predict(inputs=batches[0])

    return PredictionResponse(data={"predictions": predictions.numpy()})
