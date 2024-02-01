import argparse
import os
import pickle

import pandas
from keras import models, layers, losses, optimizers, metrics

from pkg.storage import storage
from ml.lstm.helpers import helpers


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
    '--model_dir',
    type=str,
    dest='model_dir',
)

parser.add_argument(
    '--epochs',
    type=int,
    dest='epochs',
)

parser.add_argument(
    '--days',
    type=int,
    dest='days',
)

parser.add_argument(
    '--available-tickers',
    type=str,
    dest='available_tickers',
)

arguments = parser.parse_args()

features_count = len(helpers.FEATURES)
output_length = helpers.WINDOW_OUTPUT_LENGTH


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

equity_bars = pandas.concat(
    list(equity_bars_by_year.values()),
)

available_tickers = arguments.available_tickers.split(',')

filtered_equity_bars = equity_bars[equity_bars['ticker'].isin(
    available_tickers
)]

most_recent_filtered_equity_bars = filtered_equity_bars.groupby('ticker').apply(
    lambda group: group.nlargest(arguments.days, 'timestamp')
).reset_index(drop=True)

preprocessed_data = helpers.preprocess_training_data(
    data=most_recent_filtered_equity_bars,
)

model = models.Sequential(
    layers=[
        layers.LSTM(
            units=32,
            return_sequences=False,
        ),
        layers.Dense(
            # features * days
            units=features_count * output_length
        ),
        layers.Reshape(
            # days, features
            target_shape=(output_length, features_count),
        ),
    ],
    name='basic_lstm',
)

model.compile(
    loss=losses.MeanSquaredError(),
    optimizer=optimizers.Adam(),
    metrics=[
        metrics.MeanAbsoluteError(),
    ],
)

history = model.fit(
    x=preprocessed_data['data']['training'],
    epochs=arguments.epochs,
    validation_data=preprocessed_data['data']['validating'],
)

model.save(
    filepath=os.path.join(arguments.model_dir, 'lstm.keras'),
)

metrics_file = open(
    file=os.path.join(arguments.model_dir, 'metrics.pkl'),
    mode='wb',
)

pickle.dump(
    obj=history.history,
    file=metrics_file,
)

scalers_file = open(
    file=os.path.join(arguments.model_dir, 'scalers.pkl'),
    mode='wb',
)

pickle.dump(
    obj=preprocessed_data['scalers'],
    file=scalers_file,
)

preprocessed_data['data']['testing'].save(
    path=os.path.join(arguments.model_dir, 'testing_data'),
    compression='GZIP',
)
