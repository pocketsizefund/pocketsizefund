import pandas
from sklearn import preprocessing
import numpy


DAYS_TO_TRAIN = 30


class Client:
    def __init__(self) -> None:
        self.model = None

    def preprocess_data(
        self,
        data: pandas.DataFrame,
        training_percentage: float,
    ) -> dict[str: numpy.ndarray]:
        data.sort_values(
            by='timestamp',
            inplace=True,
        )

        data_grouped_by_ticker: pandas.DataFrame = data.groupby(by='ticker')

        def create_sequence(data, sequence_length):
            input_data = []
            output_data = []
            for i in range(len(data) - sequence_length):
                input_data.append(data[i:i+sequence_length])
                output_data.append(
                    data.iloc[i+sequence_length]['close_price']
                )

            return numpy.array(input_data), numpy.array(output_data)

        preprocessed_data: dict[str: dict[str: numpy.ndarray]] = {}

        scaled_columns = [
            'open_price',
            'high_price',
            'low_price',
            'close_price',
            'volume',
        ]

        for ticker, ticker_data in data_grouped_by_ticker:
            split = int(len(ticker_data) * training_percentage)
            training_data = ticker_data[:split]
            testing_data = ticker_data[split:]

            scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))

            scaled_training_data: pandas.DataFrame = training_data.copy()
            scaled_testing_data: pandas.DataFrame = testing_data.copy()

            scaled_training_data[scaled_columns] = scaler.fit_transform(
                training_data[scaled_columns]
            )

            scaled_testing_data[scaled_columns] = scaler.transform(
                testing_data[scaled_columns]
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
