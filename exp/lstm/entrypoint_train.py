import argparse
import os
import pickle

import pandas
from keras import models, layers, losses, optimizers, metrics

from pkg.storage import storage
import entrypoint_helpers


HYPERPARAMETER_EPOCHS = 10

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

equity_bars = pandas.concat(
    list(equity_bars_by_year.values()),
)

preprocessed_data = entrypoint_helpers.preprocess_training_data(
    data=equity_bars,
)

model = models.Sequential(
    layers=[
        layers.LSTM(
            units=32,
            return_sequences=False,
        ),
        layers.Dense(
            # features * days
            units=1 * entrypoint_helpers.WINDOW_OUTPUT_LENGTH,
        ),
        layers.Reshape(
            # days, features
            target_shape=(entrypoint_helpers.WINDOW_OUTPUT_LENGTH, 1),
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
    epochs=HYPERPARAMETER_EPOCHS,
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
