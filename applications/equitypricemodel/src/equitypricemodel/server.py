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
    tide_model = Model.load()

    datamanager_response = requests.get(
        url=os.getenv("PSF_DATAMANAGER_URL", ""),
        timeout=60,
    )

    tide_data = Data()

    tide_data.preprocess_and_set_data(data=pl.read_json(datamanager_response.json()))

    batches = tide_data.get_batches(data_type="predict")

    predictions = tide_model.predict(inputs=batches[0])

    return PredictionResponse(data={"predictions": predictions.numpy()})
