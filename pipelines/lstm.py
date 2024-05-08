import os
from tempfile import NamedTemporaryFile

import polars as pl
import tensorflow
from keras import layers, losses, metrics, models, optimizers
from keras.utils import timeseries_dataset_from_array
from prefect import get_run_logger, task

import wandb
from pipelines.types import TimeWindow
from wandb.keras import WandbMetricsLogger


@task
def shape_timeseries_dataset(
    *,
    data: pl.DataFrame,
    features: list[str],
    close_price_index: int,
    window: TimeWindow,
    stride: int = 1,
    shuffle: bool = False,
    batch_size: int = 8,
) -> tensorflow.data.Dataset:
    def split_window(
        data: tensorflow.Tensor,
    ) -> tensorflow.data.Dataset:
        input_slice = slice(0, window.input)
        labels_slice = slice(window.input, None)

        inputs = data[:, input_slice, :]
        labels = data[:, labels_slice, :]

        labels = tensorflow.stack(
            [labels[:, :, close_price_index]],
            axis=-1,
        )

        inputs.set_shape([None, window.input, None])
        labels.set_shape([None, window.output, None])

        return (inputs, labels)

    return timeseries_dataset_from_array(
        data=data.select(features).to_numpy(),
        targets=None,
        sequence_length=window.length,
        sequence_stride=stride,
        shuffle=shuffle,
        batch_size=batch_size,
    ).map(split_window)


@task
def create_model(label_count, window: TimeWindow):
    model = models.Sequential(
        layers=[
            layers.LSTM(units=32, return_sequences=False),
            layers.Dense(units=label_count * window.output),
            layers.Reshape(target_shape=(window.output, label_count)),
        ],
        name="basic_lstm",
    )

    model.compile(
        loss=losses.MeanSquaredError(),
        optimizer=optimizers.Adam(),
        metrics=[
            metrics.MeanAbsoluteError(),
        ],
    )

    return model


@task
def train_model(model, train, validation, epochs: int = 10):
    wandb.login(key=os.getenv("WANDB_API_KEY"))
    wandb.init(
        project="pocketsizefund-tickerprediction",
        config={
            "epochs": epochs,
        },
        tags=["test", "local"],
    )

    model.fit(
        x=train,
        epochs=epochs,
        validation_data=validation,
        callbacks=[
            WandbMetricsLogger(),
            # WandbModelCheckpoint(
            # "models"
            # ),  # seems to be an error with the options kwarg
        ],
    )
    return model


@task
def save_model(model):
    with NamedTemporaryFile(mode="w+", delete=True) as tmp:
        filename = f"{tmp}.keras"
        model.save(filename)
        model_artifact = wandb.Artifact(name="basic-lstm", type="model")
        model_artifact.add_file(filename)
        wandb.log_artifact(model_artifact)


@task
def evaluate_model(
    model,
    data: tensorflow.data.Dataset,
) -> dict[str, any]:
    logger = get_run_logger()
    evaluation = model.evaluate(
        x=data,
        return_dict=True,
        verbose=0,
    )

    logger.info(evaluation)
