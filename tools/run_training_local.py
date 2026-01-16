"""Run model training locally instead of on SageMaker.

Downloads training data from S3, runs the trainer, uploads artifacts back to S3.
"""

import os
import sys
import tempfile
from pathlib import Path
from tinygrad import Device

import boto3
import structlog

logger = structlog.get_logger()

logger.info(f"Using device: {Device.DEFAULT}")



def download_training_data(
    s3_client: boto3.client,
    bucket_name: str,
    s3_key: str,
    local_path: Path,
) -> None:
    """Download training data from S3."""
    logger.info("Downloading training data", bucket=bucket_name, key=s3_key)

    local_path.parent.mkdir(parents=True, exist_ok=True)
    s3_client.download_file(bucket_name, s3_key, str(local_path))

    logger.info("Downloaded training data", local_path=str(local_path))


def upload_model_artifacts(
    s3_client: boto3.client,
    bucket_name: str,
    local_dir: Path,
    s3_prefix: str,
) -> None:
    """Upload model artifacts to S3."""
    logger.info("Uploading model artifacts", local_dir=str(local_dir), s3_prefix=s3_prefix)

    for file_path in local_dir.iterdir():
        if file_path.is_file():
            s3_key = f"{s3_prefix}/{file_path.name}"
            logger.info("Uploading file", file=file_path.name, s3_key=s3_key)
            s3_client.upload_file(str(file_path), bucket_name, s3_key)

    logger.info("Uploaded model artifacts")


def run_training_local(
    bucket_name: str,
    training_data_key: str = "training/filtered_tft_training_data.parquet",
    artifacts_prefix: str = "artifacts/local",
) -> None:
    """Run training locally."""
    logger.info("Starting local training", bucket=bucket_name)

    s3_client = boto3.client("s3")

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)

        # Set up paths to mimic SageMaker structure
        input_dir = temp_path / "input" / "data" / "train"
        output_dir = temp_path / "model"
        input_dir.mkdir(parents=True, exist_ok=True)
        output_dir.mkdir(parents=True, exist_ok=True)

        training_data_path = input_dir / "filtered_tft_training_data.parquet"

        # Download training data
        download_training_data(
            s3_client=s3_client,
            bucket_name=bucket_name,
            s3_key=training_data_key,
            local_path=training_data_path,
        )

        # Import and run training
        logger.info("Running trainer")

        # Patch the paths used by trainer.py
        import polars as pl

        from equitypricemodel.tide_data import Data
        from equitypricemodel.tide_model import Model

        configuration = {
            "architecture": "TiDE",
            "learning_rate": 0.003,
            "epoch_count": 20,
            "validation_split": 0.8,
            "input_length": 35,
            "output_length": 7,
            "hidden_size": 64,
            "num_encoder_layers": 2,
            "num_decoder_layers": 2,
            "dropout_rate": 0.1,
            "batch_size": 256,
        }

        training_data = pl.read_parquet(training_data_path)
        logger.info("Loaded training data", rows=training_data.height)

        tide_data = Data()
        tide_data.preprocess_and_set_data(data=training_data)

        dimensions = tide_data.get_dimensions()
        logger.info("Data dimensions", **dimensions)

        train_batches = tide_data.get_batches(
            data_type="train",
            validation_split=configuration["validation_split"],
            input_length=configuration["input_length"],
            output_length=configuration["output_length"],
            batch_size=configuration["batch_size"],
        )

        sample_batch = train_batches[0]
        batch_size = sample_batch["encoder_continuous_features"].shape[0]

        encoder_continuous_size = (
            sample_batch["encoder_continuous_features"].reshape(batch_size, -1).shape[1]
        )
        encoder_categorical_size = (
            sample_batch["encoder_categorical_features"].reshape(batch_size, -1).shape[1]
        )
        decoder_categorical_size = (
            sample_batch["decoder_categorical_features"].reshape(batch_size, -1).shape[1]
        )
        static_categorical_size = (
            sample_batch["static_categorical_features"].reshape(batch_size, -1).shape[1]
        )

        input_size = int(
            encoder_continuous_size
            + encoder_categorical_size
            + decoder_categorical_size
            + static_categorical_size
        )

        logger.info("Creating model", input_size=input_size)

        tide_model = Model(
            input_size=input_size,
            hidden_size=configuration["hidden_size"],
            num_encoder_layers=configuration["num_encoder_layers"],
            num_decoder_layers=configuration["num_decoder_layers"],
            output_length=configuration["output_length"],
            dropout_rate=configuration["dropout_rate"],
            quantiles=[0.1, 0.5, 0.9],
        )

        logger.info("Training model", epochs=configuration["epoch_count"])

        losses = tide_model.train(
            train_batches=train_batches,
            epochs=configuration["epoch_count"],
            learning_rate=configuration["learning_rate"],
        )

        logger.info("Training complete", final_loss=losses[-1] if losses else None)

        # Save model artifacts locally
        tide_model.save(directory_path=str(output_dir))
        tide_data.save(directory_path=str(output_dir))

        logger.info("Saved model artifacts locally", output_dir=str(output_dir))

        # Upload artifacts to S3
        upload_model_artifacts(
            s3_client=s3_client,
            bucket_name=bucket_name,
            local_dir=output_dir,
            s3_prefix=artifacts_prefix,
        )

    logger.info("Local training complete")


if __name__ == "__main__":
    bucket_name = os.getenv("AWS_S3_MODEL_ARTIFACTS_BUCKET")

    if not bucket_name:
        logger.error("AWS_S3_MODEL_ARTIFACTS_BUCKET environment variable not set")
        sys.exit(1)

    try:
        run_training_local(bucket_name=bucket_name)
    except Exception as e:
        logger.exception("Local training failed", error=str(e))
        sys.exit(1)
