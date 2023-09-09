import pandas
from sklearn import preprocessing
import numpy
import tensorflow


DAYS_TO_TRAIN = 30


class Client:
    def __init__(
        self,
        sort_column: str,  # timestamp
        drop_columns: list[str],  # source
        scale_columns: list[str],
        group_column: str,  # ticker
        target_column: str,  # close_price
    ) -> None:
        self.sort_column = sort_column
        self.drop_columns = drop_columns
        self.scale_columns = scale_columns
        self.group_column = group_column
        self.target_column = target_column
        self.model = None

    def preprocess_data(
        self,
        data: pandas.DataFrame,
        training_percentage: float,
    ) -> dict[str: dict[str: numpy.ndarray]]:
        data = data.drop(
            columns=self.drop_columns,
            axis=1,
        )

        data.sort_values(
            by=self.sort_column,
            inplace=True,
        )

        data_grouped_by_ticker: pandas.DataFrame = data.groupby(
            by=self.group_column,
        )

        def create_sequence(data, sequence_length):
            input_data = []
            output_data = []
            for i in range(len(data) - sequence_length):
                input_data.append(data[i:i+sequence_length])
                output_data.append(
                    data.iloc[i+sequence_length][self.target_column]
                )

            return numpy.array(input_data), numpy.array(output_data)

        preprocessed_data: dict[str: dict[str: numpy.ndarray]] = {}

        for ticker, ticker_data in data_grouped_by_ticker:
            split = int(len(ticker_data) * training_percentage)
            training_data = ticker_data[:split]
            testing_data = ticker_data[split:]

            scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))

            scaled_training_data: pandas.DataFrame = training_data.copy()
            scaled_testing_data: pandas.DataFrame = testing_data.copy()

            scaled_training_data[self.scale_columns] = scaler.fit_transform(
                training_data[self.scale_columns]
            )

            scaled_testing_data[self.scale_columns] = scaler.transform(
                testing_data[self.scale_columns]
            )

            input_training_data, output_training_data = create_sequence(
                scaled_training_data,
                DAYS_TO_TRAIN,
            )

            input_testing_data, output_testing_data = create_sequence(
                scaled_testing_data,
                DAYS_TO_TRAIN,
            )

            preprocessed_data[ticker] = {
                'input_training_data': input_training_data,
                'output_training_data': output_training_data,
                'input_testing_data': input_testing_data,
                'output_testing_data': output_testing_data,
            }

        return preprocessed_data

    def train_model(self) -> None:
        model = tensorflow.keras.models.Sequential()

        feature_count = len(self.scale_columns)
        + self.sort_column
        + self.group_column
        + self.target_column

        model.add(
            tensorflow.keras.layers.LSTM(
                units=100,
                return_sequences=True,
                input_shape=(DAYS_TO_TRAIN, feature_count),
            )
        )
        model.add(
            tensorflow.keras.layers.LSTM(
                units=50,
                return_sequences=True,
            ),
        )
        model.add(
            tensorflow.keras.layers.LSTM(
                units=50,
                return_sequences=True,
            ),
        )
        model.add(tensorflow.keras.layers.Dense(units=1))

        model.compile(
            loss='mean_squared_error',
            optimizer='adam',
        )

        self.model = model

    def save_model(
        self,
        directory_path: str,
    ) -> None:
        if self.model is None:
            raise Exception('no model to save')

        self.model.save(filepath=directory_path)

    def evaluate_model(
        self,
        test_data: numpy.ndarray,
        target_data: numpy.ndarray,
    ) -> dict[str, any]:
        loss, accuracy = self.model.evaluate(
            test_data,
            target_data,
        )

        return {
            'loss': loss,
            'accuracy': accuracy,
        }
