"""Pipeline to train and save a price prediction model."""

import datetime
from pathlib import Path

import pandas as pd
from pocketsizefund.config import config
from pocketsizefund.model import model
from pocketsizefund.storage import storage
from prefect import flow, task


@task
def download_data(
    s3_data_bucket_name: str,
    s3_artifacts_bucket_name: str,
) -> pd.DataFrame:
    """Download the raw equity bars training data."""
    storage_client = storage.Client(
        s3_data_bucket_name=s3_data_bucket_name,
        s3_artifacts_bucket_name=s3_artifacts_bucket_name,
    )

    file_names = storage_client.list_file_names(
        prefix=storage.PREFIX_EQUITY_BARS_RAW_PATH,
    )

    file_names = sorted(file_names, reverse=True)

    file_name = file_names[0]

    equity_bars_raw_dataframes = storage_client.load_dataframes(
        prefix=storage.PREFIX_EQUITY_BARS_RAW_PATH,
        file_names=[file_name],
    )

    return equity_bars_raw_dataframes[file_name]


@task
def train_model(
    data: pd.DataFrame,
) -> model.PriceModel:
    """Train the price prediction model."""
    weights_and_biases_api_key = None
    with Path("etc/.env.development").open() as config_file:
        for line in config_file:
            key, value = line.strip().split("=", 1)
            if key.strip() == "weights_and_biases_api_key":
                weights_and_biases_api_key = value.strip()
                break

    price_model = model.PriceModel(
        weights_and_biases_api_key=weights_and_biases_api_key,
    )

    price_model.train_model(
        data=data,
    )

    return price_model


@task
def save_model(
    price_model: model.PriceModel,
) -> None:
    """Save the price prediction model to local file."""
    now = datetime.datetime.now(tz=config.TIMEZONE)

    tag = now.strftime("%Y-%m-%d-%H-%M-%S")

    price_model.save_model(
        file_path=f"{tag}-model.ckpt",
    )


@flow
def pipeline(
    s3_data_bucket_name: str,
    s3_artifacts_bucket_name: str,
) -> None:
    """Pipeline to train and save a price prediction model."""
    data = download_data(
        s3_data_bucket_name=s3_data_bucket_name,
        s3_artifacts_bucket_name=s3_artifacts_bucket_name,
    )

    price_model = train_model(
        data=data,
    )

    save_model(
        price_model=price_model,
    )


if __name__ == "__main__":
    sam_config = config.SAMConfig(
        file_path="samconfig.toml",
    )

    s3_data_bucket_name = sam_config.get_parameter("S3DataBucketName")
    s3_artifacts_bucket_name = sam_config.get_parameter("S3ArtifactsBucketName")

    pipeline(
        s3_data_bucket_name=s3_data_bucket_name,
        s3_artifacts_bucket_name=s3_artifacts_bucket_name,
    )
