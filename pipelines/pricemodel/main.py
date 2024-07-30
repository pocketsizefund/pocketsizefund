"""Pipeline to train and save a price prediction model."""

import datetime
import os
from pathlib import Path

import pandas as pd
from pocketsizefund import model
from prefect import flow, task


@task
def load_data(path: Path) -> pd.DataFrame:
    return pd.read_csv(path)


@task
def train_model(
    data: pd.DataFrame,
) -> model.PriceModel:
    """Train the price prediction model."""
    WANDB_API_KEY = os.getenv("WEIGHTS_AND_BIASES_API_KEY")

    price_model = model.PriceModel(weights_and_biases_api_key=WANDB_API_KEY)

    price_model.train_model(
        data=data,
    )

    return price_model


@flow
def pipeline() -> None:
    """Pipeline to train and save a price prediction model."""
    data = load_data("data.csv")

    price_model = train_model(
        data=data,
    )


if __name__ == "__main__":
    pipeline()
