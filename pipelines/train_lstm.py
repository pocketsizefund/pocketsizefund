import os
import pickle

from pathlib import Path

import polars as pl
from sklearn import preprocessing
import tensorflow
import keras
from keras import models, layers, losses, optimizers, metrics
import wandb
import numpy as np
import pandas as pd

from prefect import task, flow
from prefect_ray import RayTaskRunner

CLOSE_PRICE_INDEX = 3


ColumnSubset = list[str]


@task(retries=3)
def load_dataset(path: Path):
    return pl.read_csv(path)


@task
def drop_nulls(data: pl.DataFrame, subset: ColumnSubset | None = None) -> pl.DataFrame:
    if subset:
        return data.drop_nulls(subset=subset)

    return data.drop_nulls()


@task
def drop_duplicates(
    data: pl.DataFrame, subset: ColumnSubset | None = None
) -> pl.DataFrame:
    if subset:
        return data.unique(subset=subset)
    return data.unique()


@task
def select_columns(
    data: pl.DataFrame, subset: ColumnSubset | None = None
) -> pl.DataFrame:
    if len(subset) == 1:
        return data.select(subset).to_numpy().reshape(-1).tolist()
    return data.select(subset)


@task
def valid_size(data, window: tuple[int, int]):
    return data.shape[0] >= (window[0] + window[1])


@task
def min_max_scaler(
    data: pl.DataFrame, feature_range: tuple[float, float] = (0, 1)
) -> pl.DataFrame:
    def _scaler(column, feature_range: tuple[float, float] = (0, 1)):
        min_ = column.min()
        max_ = column.max()
        scaled_column = (column - min_) / (max_ - min_)
        return scaled_column * (feature_range[1] - feature_range[0]) + feature_range[0]

    return data.select(
        [
            _scaler(col, feature_range) if col.dtype in [pl.Float64, pl.Int64] else col
            for col in data
        ]
    )


@task
def train_val_test_split(
    data: pl.DataFrame, 
    splits: tuple[int, int, int], preserve_order: bool = True
):
    if not preserve_order:
        data = data.shuffle()

    total_rows = data.shape[0]

    first_split = int(total_rows * splits[0])
    second_split = int(total_rows * splits[1])

    return (
        data.head(first_split),
        data.slice(first_split, first_split + second_split),
        data.tail(total_rows - first_split - second_split),
    )


@task
def group_data(data: pl.DataFrame, subset: ColumnSubset):
    return data.groupby(subset)


@task
def filter_data_by_column_value(
    data: pl.DataFrame, column: str, value: str
) -> pl.DataFrame:
    return data.filter(pl.col(column) == value)


@task
def create_model(label_count, window_output_length):
    return models.Sequential(
        layers=[
            layers.LSTM(units=32, return_sequences=False),
            layers.Dense(units=label_count * window_output_length),
            layers.Reshape(target_shape=(window_output_length, label_count)),
        ],
        name="basic_lstm",
    ).compile(
        loss=losses.MeanSquaredError(),
        optimizer=optimizers.Adam(),
        metrics=[
            metrics.MeanAbsoluteError(),
        ],
    )


@task
def train_model(model, features, epochs: int = 10):
    history = model.fit(
        x=features["training"],
        epochs=epochs,
        validation_data=features["validating"],
    )

    loss = history.history["loss"]
    mean_absolute_error = history.history["mean_absolute_error"]
    validation_loss = history.history["val_loss"]
    validation_mean_absolute_error = history.history["val_mean_absolute_error"]

    for index in range(epochs):
        wandb.log(
            {
                "training loss": loss[index],
                "training mean absolute error": mean_absolute_error[index],
                "validation loss": validation_loss[index],
                "validation mean absolute error": validation_mean_absolute_error[index],
            }
        )

    return {
        "model": model,
        "metrics": history,
    }


@flow
def train_ticker(
    data: pl.DataFrame,
    ticker: str,
    window_input_length: float = 30,
    window_output_length: float = 5,
    splits: tuple[float, float, float] = (0.7, 0.2, 0.1),
    log_prints=True,
):
    ticker_data = filter_data_by_column_value(data, column="ticker", value=ticker)

    if not valid_size(ticker_data, window=(window_input_length, window_output_length)):
        raise ValueError(
            f"{ticker_data=} has insufficient data to train on, requires minimum {window_input_length + window_output_length} observations"
        )

    # TODO validator that there's no overlap
    train, val, test = train_val_test_split(ticker_data, splits=splits)

    train = min_max_scaler(train)
    val = min_max_scaler(val)
    test = min_max_scaler(test)


@flow
def pipeline(
    timestamp_field: str = "timestamp",
    ticker_field: str = "ticker",
    features: ColumnSubset = [
        "open_price",
        "high_price",
        "low_price",
        "close_price",
        "volume",
    ],
    close_price_index: float = 3,
    log_prints=True,
    task_runner=RayTaskRunner(),
    # result_storage="s3://"
):
    data = load_dataset("test_data.csv")
    # TODO make sure ordered by time
    data = drop_nulls(data)
    data = drop_duplicates(data, subset=[timestamp_field, ticker_field])
    data = select_columns(data, subset=[timestamp_field, ticker_field] + features)

    unique_tickers = select_columns(
        drop_duplicates(data, subset=[ticker_field]), subset=[ticker_field]
    )

    for ticker in unique_tickers:
        train_ticker(data, ticker)


if __name__ == "__main__":
    pipeline()


class Model:
    def __init__(
        self,
        artifact_output_path: str,
        weights_and_biases_api_key: str,
        notes: str = "",
    ) -> None:
        self.label_count = 1

        self.artifact_output_path = artifact_output_path
        # explicit authorization instead of local secrets.env reference
        self.weights_and_biases_api_key = weights_and_biases_api_key
        self.notes = notes

        self.model = None
        self.scalers = None

    def _create_dataset(
        self,
        data: np.ndarray,
    ) -> tensorflow.data.Dataset:
        dataset = keras.utils.timeseries_dataset_from_array(
            data=data,
            targets=None,
            sequence_length=self.window_output_length + self.window_input_length,
            sequence_stride=1,
            shuffle=True,
            batch_size=32,
        )

        windowed_dataset = dataset.map(lambda x: self._split_window(x))

        return windowed_dataset

    def _split_window(
        self,
        data: tensorflow.Tensor,
    ) -> tensorflow.data.Dataset:
        input_slice = slice(0, self.window_input_length)
        labels_slice = slice(self.window_input_length, None)

        inputs = data[:, input_slice, :]
        labels = data[:, labels_slice, :]

        labels = tensorflow.stack(
            [labels[:, :, CLOSE_PRICE_INDEX]],
            axis=-1,
        )

        inputs.set_shape([None, self.window_input_length, None])
        labels.set_shape([None, self.window_output_length, None])

        return (inputs, labels)

    def preprocess_predicting_features(
        self,
        data: pd.DataFrame,
        scalers: dict[str, preprocessing.MinMaxScaler],
    ) -> dict[str, tensorflow.data.Dataset]:
        data_grouped_by_ticker = self._clean_and_group_data(data)

        predicting_datasets: dict[str, tensorflow.data.Dataset] = {}

        for ticker, ticker_data in data_grouped_by_ticker.items():
            count = len(ticker_data)

            if count < self.window_input_length:
                continue

            if ticker not in scalers:
                continue

            scaled_ticker_data = scalers[ticker].transform(
                X=ticker_data.values,
            )

            dataset = keras.utils.timeseries_dataset_from_array(
                data=scaled_ticker_data,
                targets=None,
                sequence_length=self.window_input_length,
                shuffle=True,
            )

            predicting_datasets[ticker] = dataset

        return predicting_datasets

    def train_model(
        self,
        features: dict[str, pd.DataFrame],
        epochs: int = 10,
    ) -> dict[str, any]:
        wandb.login(key=self.weights_and_biases_api_key)

        wandb.init(
            project="basic-lstm",
            config={
                "epochs": epochs,
            },
            notes=self.notes,
        )

    def evaluate_model(
        self,
        data: tensorflow.data.Dataset,
    ) -> dict[str, any]:
        evaluation = self.model.evaluate(
            x=data,
            return_dict=True,
            verbose=0,
        )

        return {
            "loss": evaluation["loss"],
            "mean_absolute_error": evaluation["mean_absolute_error"],
        }

    def save_model(
        self,
        model: models.Sequential,
    ) -> None:
        model.save(
            filepath=os.path.join(self.artifact_output_path, "lstm.keras"),
        )

    def save_metrics(
        self,
        metrics: any,
    ) -> None:
        metrics_file = open(
            file=os.path.join(self.artifact_output_path, "metrics.pkl"),
            mode="wb",
        )

        pickle.dump(
            obj=metrics,
            file=metrics_file,
        )

    def save_scalers(
        self,
        scalers: dict[str, preprocessing.MinMaxScaler],
    ) -> None:
        with open(
            file=os.path.join(self.artifact_output_path, "scalers.pkl"),
            mode="wb",
        ) as scalers_file:
            pickle.dump(
                obj=scalers,
                file=scalers_file,
            )

    def save_data(
        self,
        name: str,
        data: tensorflow.data.Dataset,
    ) -> None:
        data.save(
            path=os.path.join(self.artifact_output_path, name),
            compression="GZIP",
        )

    def load_model(
        self,
    ) -> None:
        self.model = models.load_model(
            filepath=os.path.join(self.artifact_output_path, "lstm.keras"),
        )

    def load_scalers(
        self,
    ) -> None:
        scalers_file_path = os.path.join(
            self.artifact_output_path,
            "scalers.pkl",
        )

        with open(scalers_file_path, "rb") as scalers_file:
            self.scalers = pickle.load(file=scalers_file)

    def generate_predictions(
        self,
        features: dict[str, pd.DataFrame],
    ) -> dict[str, list[float]]:
        if not self.model or not self.scalers:
            raise Exception("no model or scalers")

        predictions: dict[str, any] = {}
        for ticker, ticker_features in features.items():
            prediction = self.model.predict(
                x=ticker_features,
                verbose=0,
            )

            prediction = np.squeeze(prediction, axis=0)

            scaler = self.scalers[ticker]

            minimum_value = scaler.data_min_[CLOSE_PRICE_INDEX]
            maximum_value = scaler.data_max_[CLOSE_PRICE_INDEX]

            unscaled_predictions = (
                prediction * (maximum_value - minimum_value)
            ) + minimum_value

            predictions[ticker] = unscaled_predictions.tolist()

        return predictions
