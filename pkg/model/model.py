import os
import pickle

from sagemaker import tensorflow as sagemaker_tensorflow
import pandas
from sklearn import preprocessing
import numpy
import tensorflow
import keras
from keras import models, layers, losses, optimizers, metrics


FEATURE_NAMES = tuple(
    [
        'open_price',
        'high_price',
        'low_price',
        'close_price',
        'volume',
    ]
)

REQUIRED_COLUMNS = tuple(
    [
        'timestamp',
        'ticker',
    ]
)

WINDOW_INPUT_LENGTH = 30
WINDOW_OUTPUT_LENGTH = 5

CLOSE_PRICE_INDEX = 3


class Model:
    def __init__(
        self,
        artifact_output_path: str,
    ) -> None:
        self.feature_names = FEATURE_NAMES
        self.required_columns = REQUIRED_COLUMNS
        self.window_input_length = WINDOW_INPUT_LENGTH
        self.window_output_length = WINDOW_OUTPUT_LENGTH
        self.label_count = 1

        self.artifact_output_path = artifact_output_path

        self.model = None
        self.scalers = None

    def preprocess_training_features(
        self,
        data: pandas.DataFrame,
        splits: tuple[float, float, float] = (0.7, 0.2, 0.1),
    ) -> dict[str, any]:
        data_grouped_by_ticker = self._clean_and_group_data(data)

        scalers: dict[int, preprocessing.MinMaxScaler] = {}

        scaled_training_data: list[numpy.ndarray] = []
        scaled_validating_data: list[numpy.ndarray] = []
        scaled_testing_data: list[numpy.ndarray] = []

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

            scaled_training_data.append(scaler.fit_transform(
                X=training_data.values,
            ))

            scaled_validating_data.append(scaler.transform(
                X=validating_data.values,
            ))

            scaled_testing_data.append(scaler.transform(
                X=testing_data.values,
            ))

            scalers[ticker] = scaler

        training_datasets = list(map(lambda x: self._create_dataset(
            data=x,
        ), scaled_training_data))

        validating_datasets = list(map(lambda x: self._create_dataset(
            data=x,
        ), scaled_validating_data))

        testing_datasets = list(map(lambda x: self._create_dataset(
            data=x,
        ), scaled_testing_data))

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
            'data': {
                'training': training_dataset,
                'validating': validating_dataset,
                'testing': testing_dataset,
            },
            'scalers': scalers,
        }

    def _create_dataset(
        self,
        data: numpy.ndarray,
    ) -> tensorflow.data.Dataset:
        dataset = keras.utils.timeseries_dataset_from_array(
            data=data,
            targets=None,
            sequence_length=self.window_output_length+self.window_input_length,
            sequence_stride=1,
            shuffle=True,
            batch_size=32,
        )

        windowed_dataset = dataset.map(
            lambda x: self._split_window(x)
        )

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
        data: pandas.DataFrame,
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

    def _clean_and_group_data(
        self,
        data: pandas.DataFrame,
    ) -> dict[str, pandas.DataFrame]:
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
            for ticker, ticker_group
            in data.groupby(
                by=ticker_column,
                dropna=True,
            )
        }

        return data_grouped_by_ticker

    def train_model(
        self,
        features: dict[str, pandas.DataFrame],
        epochs: int = 10,
    ) -> dict[str, any]:
        self.model = models.Sequential(
            layers=[
                layers.LSTM(
                    units=32,
                    return_sequences=False,
                ),
                layers.Dense(
                    # labels * days
                    units=self.label_count * self.window_output_length
                ),
                layers.Reshape(
                    # days, labels
                    target_shape=(self.window_output_length, self.label_count),
                ),
            ],
            name='basic_lstm',
        )

        self.model.compile(
            loss=losses.MeanSquaredError(),
            optimizer=optimizers.Adam(),
            metrics=[
                metrics.MeanAbsoluteError(),
            ],
        )

        history = self.model.fit(
            x=features['training'],
            epochs=epochs,
            validation_data=features['validating'],
        )

        return {
            'model': self.model,
            'metrics': history.history,
        }

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
            'loss': evaluation['loss'],
            'mean_absolute_error': evaluation['mean_absolute_error'],
        }

    def save_model(
        self,
        model: models.Sequential,
    ) -> None:
        model.save(
            filepath=os.path.join(self.artifact_output_path, 'lstm.keras'),
        )

    def save_metrics(
        self,
        metrics: any,
    ) -> None:
        metrics_file = open(
            file=os.path.join(self.artifact_output_path, 'metrics.pkl'),
            mode='wb',
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
            file=os.path.join(self.artifact_output_path, 'scalers.pkl'),
            mode='wb',
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
            compression='GZIP',
        )

    def load_model(
        self,
    ) -> None:
        self.model = models.load_model(
            filepath=os.path.join(self.artifact_output_path, 'lstm.keras'),
        )

    def load_scalers(
        self,
    ) -> None:
        scalers_file_path = os.path.join(
            self.artifact_output_path,
            'scalers.pkl',
        )

        with open(scalers_file_path, 'rb') as scalers_file:
            self.scalers = pickle.load(file=scalers_file)

    def generate_predictions(
        self,
        features: dict[str, pandas.DataFrame],
    ) -> dict[str, list[float]]:
        if not self.model or not self.scalers:
            raise Exception('no model or scalers')

        predictions: dict[str, any] = {}
        for ticker, ticker_features in features.items():
            prediction = self.model.predict(
                x=ticker_features,
                verbose=0,
            )

            prediction = numpy.squeeze(prediction, axis=0)

            scaler = self.scalers[ticker]

            minimum_value = scaler.data_min_[CLOSE_PRICE_INDEX]
            maximum_value = scaler.data_max_[CLOSE_PRICE_INDEX]

            unscaled_predictions = (
                prediction*(
                    maximum_value -
                    minimum_value)
            ) + minimum_value

            predictions[ticker] = unscaled_predictions.tolist()

        return predictions


class Client:
    def __init__(
        self,
        model_endpoint_name: str,
    ) -> None:
        self.predictor = sagemaker_tensorflow.TensorFlowPredictor(
            endpoint_name=model_endpoint_name,
        )

    def generate_predictions(
        self,
        data: pandas.DataFrame,
    ) -> any:
        data['timestamp'] = data['timestamp'].astype(str)

        predictions = self.predictor.predict(
            data=data.to_dict(orient='records'),
        )

        return predictions
