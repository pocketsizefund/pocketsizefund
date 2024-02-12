import pandas
from sklearn import preprocessing
import numpy
import tensorflow
import keras


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


class Client:
    def __init__(self):
        self.feature_names = FEATURE_NAMES
        self.required_columns = REQUIRED_COLUMNS
        self.window_input_length = WINDOW_INPUT_LENGTH
        self.window_output_length = WINDOW_OUTPUT_LENGTH

    def generate_features(
        self,
        data: pandas.DataFrame,
    ) -> pandas.DataFrame:
        # temporary implementation that will
        # hold feature engineering logic
        return data

    def preprocess_training_data(
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
            values=[labels],
            axis=-1,
        )

        labels = tensorflow.squeeze(labels, axis=-1)

        inputs.set_shape([None, self.window_input_length, None])
        labels.set_shape([None, self.window_output_length, None])

        return (inputs, labels)

    def preprocess_predicting_data(
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
