"""Pipeline to train and save a price prediction model."""

import datetime

import pandas as pd
from prefect import flow, task

from pkg.config import config
from pkg.model import model
from pkg.storage import storage


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
) -> model.Model:
    """Train the price prediction model."""
    price_prediction_model = model.Model()

    price_prediction_model.train_model(
        data=data,
    )

    return price_prediction_model


@task
def save_model(
    price_prediction_model: model.Model,
) -> None:
    """Save the price prediction model to local file."""
    now = datetime.datetime.now(tz=config.TIMEZONE)

    tag = now.strftime("%Y-%m-%d-%H-%M-%S")

    price_prediction_model.save_model(
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

    price_prediction_model = train_model(
        data=data,
    )

    save_model(
        price_prediction_model=price_prediction_model,
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