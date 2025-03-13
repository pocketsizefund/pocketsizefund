from fastapi import FastAPI
import requests
import os
from datetime import datetime, timedelta
from library.events import build_response_event
from pricemodel.dataset import DataSet
from pricemodel.miniature_temporal_fusion_transformer import MiniatureTemporalFusionTransformer
import polars as pl


app = FastAPI()


@app.get("/predictions")
async def prediction_handler():
    environment = os.getenv("ENVIRONMENT", "development").lower()

    data_provider_url = f"http://dataprovider.{environment}.svc.cluster.local:8080/data"

    end_at = datetime.now().date()

    start_at = end_at - timedelta(days=30)

    payload = {
        "start_at": start_at,
        "end_at": end_at,
    }

    mini_tft: MiniatureTemporalFusionTransformer = MiniatureTemporalFusionTransformer.load()

    response = requests.post(data_provider_url, params=payload).json()

    data = pl.DataFrame(response)

    dataset = DataSet(batch_size=1, sequence_length=1)

    dataset.load(data)

    for tickers, historical_features, _ in dataset:
        percentiles_25, percentiles_50, percentiles_75 = mini_tft.predict(
            tickers=tickers,
            features=historical_features,
        )

    # dataset = DataSet(batch_size=1, sequence_length=1)


# outline:
# [ ] create handler
# - [x] parse / acknowledge request
# - [x] create data fetch url
# - [x] fetch data
# - [x] parse data to dataset
# - [x] invoke model
# - [ ] parse + format result
# - [ ] emit response w/ cloud event format
# [x] create server
# - [x] read model file
# - [x] instantiate model
