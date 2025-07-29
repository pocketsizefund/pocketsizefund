from datetime import datetime
from zoneinfo import ZoneInfo

import polars as pl
from flytekit import task, workflow
from internal.dataset import TemporalFusionTransformerDataset
from internal.tft_model import Parameters, TemporalFusionTransformer
from loguru import logger

import wandb

configuration = {
    "architecture": "TFT",
    "learning_rate": 0.02,
    "epoch_count": 10,
    "validation_split": 0.8,
}


if wandb.run is not None:
    wandb.finish()  # close active run if it exists

run = wandb.init(
    project="Pocket Size Fund",
    config=configuration,
    name=f"tft-model-run-{datetime.now(tz=ZoneInfo('America/New_York')).strftime('%Y-%m-%d_%H-%M-%S')}",
)

timezone = ZoneInfo("America/New_York")


@task
def read_local_data(filepath: str) -> TemporalFusionTransformerDataset:
    start_time = datetime.now(tz=timezone)
    logger.info(f"Reading data from {filepath}")
    data = pl.read_csv(filepath)

    runtime_seconds = (datetime.now(tz=timezone) - start_time).seconds

    logger.info(f"Data read successfully in {runtime_seconds} seconds")

    return TemporalFusionTransformerDataset(data=data)


@task
def train_model(
    dataset: TemporalFusionTransformerDataset,
    validation_split: float = 0.8,
    epoch_count: int = 10,
    learning_rate: float = 1e-3,
) -> TemporalFusionTransformer:
    start_time = datetime.now(tz=timezone)
    logger.info("Training temporal fusion transformer model")
    dimensions = dataset.get_dimensions()

    parameters = Parameters(
        hidden_size=64,
        output_size=1,
        lstm_layer_count=3,
        attention_head_size=4,
        dropout_rate=0.1,
        quantiles=[0.1, 0.5, 0.9],
        decoder_categorical_dimension=dimensions["decoder_categorical_features"],
        decoder_continuous_dimension=dimensions["decoder_continuous_features"],
        encoder_categorical_dimension=dimensions["encoder_categorical_features"],
        encoder_continuous_dimension=dimensions["encoder_continuous_features"],
        static_categorical_dimension=dimensions["static_categorical_features"],
        static_continuous_dimension=dimensions["static_continuous_features"],
        input_length=35,
        output_length=7,
    )

    model = TemporalFusionTransformer(parameters=parameters)

    batches = dataset.get_batches(
        data_type="train",
        validation_split=validation_split,
        input_length=parameters.input_length,
        output_length=parameters.output_length,
    )

    logger.info(f"Training model with {len(batches)} batches")

    losses = model.train(
        inputs_list=batches,
        epoch_count=epoch_count,
        learning_rate=learning_rate,
    )

    for loss in losses:
        run.log({"loss": loss})

    run.finish()

    runtime_seconds = (datetime.now(tz=timezone) - start_time).seconds

    logger.info(f"Model trained successfully in {runtime_seconds} seconds")

    return model


@task
def validate_model(
    data: TemporalFusionTransformerDataset,
    model: TemporalFusionTransformer,
    validation_split: float = 0.8,
) -> None:
    start_time = datetime.now(tz=timezone)
    logger.info("Validating temporal fusion transformer model")

    batches = data.get_batches(
        data_type="validation",
        validation_split=validation_split,
        input_length=model.input_length,
        output_length=model.output_length,
    )

    validation_result = model.validate(
        inputs_list=batches,
    )

    runtime_seconds = (datetime.now(tz=timezone) - start_time).seconds

    logger.info(f"Validation completed in {runtime_seconds} seconds")

    logger.info(f"Validation result {validation_result}")


@task
def save_model(model: TemporalFusionTransformer) -> None:
    start_time = datetime.now(tz=timezone)
    logger.info("Saving temporal fusion transformer model")

    model.save()

    runtime_seconds = (datetime.now(tz=timezone) - start_time).seconds

    logger.info(f"Model saved successfully in {runtime_seconds} seconds")


@workflow
def train_tft_model() -> None:
    dataset = read_local_data(
        filepath="applications/models/src/models/training_data.csv"
    )  # type: ignore[assignment]

    model = train_model(
        dataset=dataset,  # type: ignore[arg-type]
        validation_split=configuration["validation_split"],
        epoch_count=configuration["epoch_count"],
        learning_rate=configuration["learning_rate"],
    )

    validate_model(
        data=dataset,  # type: ignore[arg-type]
        model=model,  # type: ignore[arg-type]
        validation_split=configuration["validation_split"],
    )

    save_model(model=model)  # type: ignore[arg-type]
