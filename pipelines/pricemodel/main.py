"""Pipeline to train and save a price prediction model."""

import os
from pathlib import Path

import pandas as pd
from pocketsizefund import model
from prefect import flow, task


@task
def load_data(path: Path) -> pd.DataFrame:
    """Load data from a local file."""
    return pd.read_csv(path)


@task
def train_model(
    data: pd.DataFrame,
) -> model.PriceModel:
    """Train the price prediction model."""
    wandb_api_key = os.getenv("WEIGHTS_AND_BIASES_API_KEY")

    price_model = model.PriceModel(weights_and_biases_api_key=wandb_api_key)

    price_model.train_model(
        data=data,
    )

    return price_model


@flow
def pipeline() -> None:
    """Pipeline to train and save a price prediction model."""
    data = load_data("data.csv")

    train_model(
        data=data,
    )


if __name__ == "__main__":
    pipeline()
