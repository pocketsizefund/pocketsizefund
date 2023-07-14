import pandas
from sklearn import preprocessing
import numpy
import keras


NO_MODEL_EXCEPTION = Exception('no model loaded on client')
NOT_ENOUGH_DATA_EXCEPTION = Exception('not enough data to make prediction')

DAYS_TO_TRAIN = 30
DAYS_TO_PREDICT = 5


class Client:
    def __init__(
        self,
        file_path: str,
    ) -> None:
        self.model = keras.models.Sequential = keras.models.load_model(
            file_path,
        )

    def train_model(
        self,
        data: pandas.DataFrame,
    ) -> None:
        target_data = data.groupby(
            'ticker',
        )['close_price'].apply(numpy.array).values

        scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))
        target_data_scaled = scaler.fit_transform(
            numpy.concatenate(target_data).reshape(-1, 1),
        )

        train_inputs = []
        train_outputs = []
        for i in range(DAYS_TO_TRAIN, len(target_data_scaled)):
            train_inputs.append(target_data_scaled[i - DAYS_TO_TRAIN:i])
            train_outputs.append(target_data_scaled[i])

        train_input = numpy.array(train_inputs)
        train_output = numpy.array(train_outputs)

        model = keras.models.Sequential()
        model.add(
            keras.layers.LSTM(
                units=50,
                return_sequences=True,
                input_shape=(train_input.shape[1], train_input.shape[2]),
            ),
        )
        model.add(keras.layers.LSTM(units=50))
        model.add(keras.layers.Dense(units=1))

        model.compile(optimizer='adam', loss='mean_squared_error')

        model.fit(train_input, train_output, epochs=10, batch_size=32)

        self.model = model

    def save_model(
        self,
        file_path: str,
    ) -> None:
        if self.model is None:
            raise NO_MODEL_EXCEPTION

        self.model.save(file_path)

    def get_model_predictions(
        self,
        data: pandas.DataFrame,
    ) -> dict[str, any]:
        if self.model is None:
            return NO_MODEL_EXCEPTION

        if len(data) < DAYS_TO_TRAIN:
            raise NOT_ENOUGH_DATA_EXCEPTION

        scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))

        target_data = data['close_price'].values
        target_data_scaled = scaler.fit_transform(target_data.reshape(-1, 1))

        predictions: dict[str, any] = {}
        for ticker in data['ticker'].unique():
            ticker_data = data[data['ticker'] == ticker]
            ticker_target_data_scaled = target_data_scaled[ticker_data.index]

            inputs = ticker_target_data_scaled[-DAYS_TO_TRAIN:]

            predicted_outputs = []
            for _ in range(DAYS_TO_PREDICT):
                input_reshaped = numpy.reshape(inputs, (1, DAYS_TO_TRAIN, 1))
                prediction = self.model.predict(input_reshaped)[0][0]
                predicted_outputs.append(prediction)

                inputs = numpy.append(inputs[1:], prediction)

            predicted_outputs_scaled = scaler.inverse_transform(
                numpy.array(predicted_outputs).reshape(-1, 1),
            )

            predictions[ticker] = predicted_outputs_scaled.flatten().tolist(),

        return predictions
