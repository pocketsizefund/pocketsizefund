import argparse
import os
import pickle

import pandas
from sklearn import preprocessing
import numpy
from keras import models, layers

from pkg.storage import storage


parser = argparse.ArgumentParser(
    prog='model training script',
    description='train the lstm model',
)

parser.add_argument(
    '--s3-data-bucket-name',
    type=str,
    dest='s3_data_bucket_name',
)

parser.add_argument(
    '--training-split',
    type=float,
    dest='training_split',
)

parser.add_argument(
    '--epochs',
    type=int,
    dest='epochs',
)

parser.add_argument(
    '--model_dir',
    type=str,
    dest='model_dir',
)

arguments = parser.parse_args()

storage_client = storage.Client(
    s3_data_bucket_name=arguments.s3_data_bucket_name,
)

file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
)

equity_bars_by_year = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
    file_names=file_names,
)

input_data = pandas.concat(
    list(equity_bars_by_year.values()),
)

input_data.set_index(
    keys='timestamp',
    inplace=True,
)

input_data.drop(
    columns=['source'],
    inplace=True,
)


def convert_ticker_to_integer(ticker: str) -> int:
    return int.from_bytes(ticker.encode(), 'little')


input_data['ticker'] = input_data['ticker'].apply(convert_ticker_to_integer)

scalers: dict[int, dict[str, preprocessing.MinMaxScaler]] = {}

preprocessed_training_data: dict[str, list[numpy.ndarray]] = {
    'input': [],
    'output': [],
}

preprocessed_testing_data: dict[int, dict[str, numpy.ndarray]] = {}

input_data_grouped_by_ticker = input_data.groupby(
    by='ticker',
    dropna=True,
)

for ticker, ticker_input_data in input_data_grouped_by_ticker:
    ticker_output_data = ticker_input_data[['close_price']].copy()

    ticker_input_data.sort_index(
        ascending=True,
        inplace=True,
    )

    ticker_input_data.drop(
        index=ticker_input_data.index[0],
        inplace=True,
    )

    ticker_output_data.sort_index(
        ascending=True,
        inplace=True,
    )

    ticker_output_data.drop(
        index=ticker_output_data.index[-1],
        inplace=True,
    )

    scalers[ticker] = {
        'input': preprocessing.MinMaxScaler(feature_range=(0, 1)),
        'output': preprocessing.MinMaxScaler(feature_range=(0, 1)),
    }

    scaled_ticker_input_data = scalers[ticker]['input'].fit_transform(
        X=ticker_input_data.values,
    )

    scaled_ticker_output_data = scalers[ticker]['output'].fit_transform(
        X=ticker_output_data.values
    )

    split = int(len(scaled_ticker_input_data) * arguments.training_split)

    scaled_training_input_data = scaled_ticker_input_data[:split]
    scaled_training_output_data = scaled_ticker_output_data[:split]
    scaled_testing_input_data = scaled_ticker_input_data[split:]
    scaled_testing_output_data = scaled_ticker_output_data[split:]

    preprocessed_training_data['input'].append(
        scaled_training_input_data
    )
    preprocessed_training_data['output'].append(
        scaled_training_output_data
    )

    preprocessed_testing_data[ticker] = {
        'input': scaled_testing_input_data,
        'output': scaled_testing_output_data,
    }

training_input_data = numpy.concatenate(
    preprocessed_training_data['input'],
    axis=0,
)

training_input_data = training_input_data.reshape(
    training_input_data.shape[0],
    1,
    training_input_data.shape[1],
)

training_output_data = numpy.concatenate(
    preprocessed_training_data['output'],
    axis=0,
)

training_output_data = training_output_data.reshape(
    training_output_data.shape[0],
    1,
    training_output_data.shape[1],
)

model = models.Sequential(
    layers=[
        layers.LSTM(
            units=100,
            return_sequences=True,
            input_shape=(
                training_input_data.shape[1],
                training_input_data.shape[2],
            ),
        ),
        layers.LSTM(
            units=50,
            return_sequences=True,
        ),
        layers.LSTM(
            units=50,
            return_sequences=True,
        ),
        layers.LSTM(
            units=50,
            return_sequences=True,
        ),
        layers.Dense(units=1),
    ],
    name='basic_lstm',
)

model.compile(
    loss='mean_squared_error',
    optimizer='adam',
)

model.fit(
    x=training_input_data,
    y=training_output_data,
    epochs=arguments.epochs,
    # validation_data=(testing_input_data, testing_output_data),
    shuffle=False,
    verbose=0,
)

model.save(
    filepath=os.path.join(arguments.model_dir, 'lstm.keras'),
)

scalers_file = open(
    file=os.path.join(arguments.model_dir, 'scalers.pkl'),
    mode='wb',
)

pickle.dump(
    obj=scalers,
    file=scalers_file,
)

for ticker, ticker_data in preprocessed_testing_data.items():
    testing_input_data = ticker_data['input']
    testing_output_data = ticker_data['output']

    testing_input_data = testing_input_data.reshape(
        testing_input_data.shape[0],
        1,
        testing_input_data.shape[1],
    )

    testing_output_data = testing_output_data.reshape(
        testing_output_data.shape[0],
        1,
        testing_output_data.shape[1],
    )

    predictions = model.predict(
        x=testing_input_data,
        verbose=0,
    )

    scaler = scalers[ticker]['output']

    unscaled_predictions = scaler.inverse_transform(
        X=numpy.squeeze(
            a=predictions,
            axis=1,
        ),
    )
