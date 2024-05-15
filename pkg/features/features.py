"""Module for generating features and preprocessing data."""

import keras
import numpy as np
import pandas as pd
import tensorflow as tf
from sklearn import preprocessing

FEATURE_NAMES = tuple(
    [
        "open_price",
        "high_price",
        "low_price",
        "close_price",
        "volume",
    ],
)

REQUIRED_COLUMNS = tuple(
    [
        "timestamp",
        "ticker",
    ],
)

WINDOW_INPUT_LENGTH = 30
WINDOW_OUTPUT_LENGTH = 5

CLOSE_PRICE_INDEX = 3


class Client:
    def __init__(self):
        """
        Initialize the Client object with the feature names, required columns,
        window input length, and window output length.

        Args:
            feature_names (tuple[str]): Feature names.
            required_columns (tuple[str]): Required columns.
            window_input_length (int): Window input length.
            window_output_length (int): Window output length.
        """
        self.feature_names = FEATURE_NAMES
        self.required_columns = REQUIRED_COLUMNS
        self.window_input_length = WINDOW_INPUT_LENGTH
        self.window_output_length = WINDOW_OUTPUT_LENGTH

    def generate_features(
        self,
        data: pd.DataFrame,
    ) -> pd.DataFrame:
        """
        A function to generate features based on the input data.

        Args:
            data (pd.DataFrame): The input DataFrame containing the data.

        Returns:
            pd.DataFrame: The DataFrame with generated features.
        """
        # temporary implementation that will
        # hold feature engineering logic
        return data

    def preprocess_training_features(
        self,
        data: pd.DataFrame,
        splits: tuple[float, float, float] = (0.7, 0.2, 0.1),
    ) -> dict[str, any]:
        """
        Preprocesses the training features for machine learning modeling.

        Args:
            data (pd.DataFrame): The input DataFrame containing the data.
            splits (tuple[float, float, float], optional): The split ratios for training, validating, and testing data. Defaults to (0.7, 0.2, 0.1).

        Returns:
            dict[str, any]: A dictionary containing the preprocessed data, including training, validating, and testing datasets, and scalers used for preprocessing.
        """
        data_grouped_by_ticker = self._clean_and_group_data(data)

        scalers: dict[int, preprocessing.MinMaxScaler] = {}

        scaled_training_data: list[np.ndarray] = []
        scaled_validating_data: list[np.ndarray] = []
        scaled_testing_data: list[np.ndarray] = []

        for ticker, ticker_data in data_grouped_by_ticker.items():
            count = len(ticker_data)

            if count < self.window_input_length + self.window_output_length:
                continue

            first_split = int(count * splits[0])
            second_split = int(count * (splits[0] + splits[1]))

            training_data = ticker_data[:first_split]
            validating_data = ticker_data[first_split:second_split]
            testing_data = ticker_data[second_split:]

            scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))

            scaled_training_data.append(
                scaler.fit_transform(
                    X=training_data.values,
                )
            )

            scaled_validating_data.append(
                scaler.transform(
                    X=validating_data.values,
                )
            )

            scaled_testing_data.append(
                scaler.transform(
                    X=testing_data.values,
                )
            )

            scalers[ticker] = scaler

        training_datasets = list(
            map(
                lambda x: self._create_dataset(
                    data=x,
                ),
                scaled_training_data,
            )
        )

        validating_datasets = list(
            map(
                lambda x: self._create_dataset(
                    data=x,
                ),
                scaled_validating_data,
            )
        )

        testing_datasets = list(
            map(
                lambda x: self._create_dataset(
                    data=x,
                ),
                scaled_testing_data,
            )
        )

        training_dataset = training_datasets[0]
        for dataset in training_datasets[1:]:
            training_dataset = training_dataset.concatenate(dataset)

        validating_dataset = validating_datasets[0]
        for dataset in validating_datasets[1:]:
            validating_dataset = validating_dataset.concatenate(dataset)

        testing_dataset = testing_datasets[0]
        for dataset in testing_datasets[1:]:
            testing_dataset = testing_dataset.concatenate(dataset)

        return {
            "data": {
                "training": training_dataset,
                "validating": validating_dataset,
                "testing": testing_dataset,
            },
            "scalers": scalers,
        }

    def _create_dataset(
        self,
        data: np.ndarray,
    ) -> tf.data.Dataset:
        """
        Creates a time series dataset from the input data array.

        Args:
            data (np.ndarray): The input data array.

        Returns:
            tf.data.Dataset: The created time series dataset.
        """
        dataset = keras.utils.timeseries_dataset_from_array(
            data=data,
            targets=None,
            sequence_length=self.window_output_length + self.window_input_length,
            sequence_stride=1,
            shuffle=True,
            batch_size=32,
        )

        windowed_dataset = dataset.map(
            lambda x: self._split_window(x),
        )

        return windowed_dataset

    def _split_window(
        self,
        data: tf.Tensor,
    ) -> tf.data.Dataset:
        """
        Splits the input data tensor into inputs and labels based on window sizes.

        Args:
            data (tf.Tensor): The input data tensor.

        Returns:
            tf.data.Dataset: The created time series dataset containing inputs and labels.
        """
        input_slice = slice(0, self.window_input_length)
        labels_slice = slice(self.window_input_length, None)

        inputs = data[:, input_slice, :]
        labels = data[:, labels_slice, :]

        labels = tf.stack(
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
    ) -> dict[str, tf.data.Dataset]:
        """
        Preprocesses the predicting features for machine learning modeling.

        Args:
            data (pd.DataFrame): The input DataFrame containing the data.
            scalers (dict[str, preprocessing.MinMaxScaler]): A dictionary of scalers for each ticker.

        Returns:
            dict[str, tf.data.Dataset]: A dictionary containing the preprocessed predicting datasets for each ticker.
        """
        data_grouped_by_ticker = self._clean_and_group_data(data)

        predicting_datasets: dict[str, tf.data.Dataset] = {}

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

    def _clean_and_group_data(
        self,
        data: pd.DataFrame,
    ) -> dict[str, pd.DataFrame]:
        """
        Cleans and groups the input data DataFrame by dropping NA values, duplicates based on timestamp and ticker column, filters the data based on required columns and feature names, sets the timestamp column as the index, and groups the data by ticker column.

        Args:
            data (pd.DataFrame): The input DataFrame to be cleaned and grouped.

        Returns:
            dict[str, pd.DataFrame]: A dictionary containing the cleaned and grouped data by ticker.
        """
        data.dropna(
            inplace=True,
        )

        timestamp_column = self.required_columns[0]
        ticker_column = self.required_columns[1]

        data.drop_duplicates(
            subset=[
                timestamp_column,
                ticker_column,
            ],
            inplace=True,
        )

        data = data.filter(
            items=self.required_columns + self.feature_names,
            axis=1,
        )

        data.set_index(
            keys=timestamp_column,
            inplace=True,
        )

        data_grouped_by_ticker = {
            str(ticker): ticker_group.drop(
                columns=[ticker_column],
            )
            for ticker, ticker_group in data.groupby(
                by=ticker_column,
                dropna=True,
            )
        }

        return data_grouped_by_ticker
