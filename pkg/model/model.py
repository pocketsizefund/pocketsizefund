import pandas
from sklearn import preprocessing
import numpy
import keras


NO_MODEL_EXCEPTION = Exception('no model loaded on client')
NOT_ENOUGH_DATA_EXCEPTION = Exception('not enough data to make prediction')

DAYS_TO_TRAIN = 30
DAYS_TO_PREDICT = 5


class Client:
    def __init__(self):
        self.model: keras.models.Sequential = None

    def train_model(
        self,
        data: pandas.DataFrame,
    ) -> keras.models.Sequential:
        target_variable = 'close_price'
        target_data = data[target_variable].values.reshape(-1, 1)

        scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))
        target_data_scaled = scaler.fit_transform(target_data)

        train_inputs = []
        train_outputs = []
        for i in range(DAYS_TO_TRAIN, len(target_data_scaled)):
            train_inputs.append(target_data_scaled[i - DAYS_TO_TRAIN:i, 0])
            train_outputs.append(target_data_scaled[i, 0])

        train_input = numpy.array(train_inputs)
        train_output = numpy.array(train_outputs)

        train_input_reshaped = numpy.reshape(
            train_input,
            (train_input.shape[0], train_input.shape[1], 1),
        )

        model = keras.models.Sequential()
        model.add(
            keras.layers.LSTM(
                units=50,
                return_sequences=True,
                input_shape=(train_input_reshaped.shape[1], 1),
            ),
        )
        model.add(keras.layers.LSTM(units=50))
        model.add(keras.layers.Dense(units=1))

        model.compile(optimizer='adam', loss='mean_squared_error')

        model.fit(train_input_reshaped, train_output, epochs=10, batch_size=32)

        self.model = model

        return model

    def save_model(
        self,
        path: str,
    ) -> None:
        if self.model is None:
            raise NO_MODEL_EXCEPTION

        self.model.save(path)

    def load_model(
        self,
        path: str,
    ) -> keras.models.Sequential:
        model = keras.models.load_model(path)

        self.model = model

        return model

    def get_model_predictions(
        self,
        data: pandas.DataFrame,
    ):  # TEMP
        if self.model is None:
            return NO_MODEL_EXCEPTION

        if len(data) < DAYS_TO_TRAIN:
            raise NOT_ENOUGH_DATA_EXCEPTION

        target_variable = 'close_price'
        target_data = data[target_variable].values.reshape(-1, 1)

        scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))
        target_data_scaled = scaler.fit_transform(target_data)
        target_data_scaled_reshaped = numpy.reshape(
            target_data_scaled,
            (
                1,
                target_data_scaled.shape[0],
                1,
            ),
        )

        predictions = []
        for _ in range(DAYS_TO_PREDICT):
            prediction = self.model.predict(target_data_scaled_reshaped)
            predictions.append(prediction)
            target_data_scaled_reshaped = numpy.append(
                target_data_scaled_reshaped[:, 1:, :],
                prediction.reshape(1, 1, 1),
                axis=1,
            )

        predictions = scaler.inverse_transform(
            numpy.array(predictions).reshape(-1, 1),
        )

        return predictions.tolist()
