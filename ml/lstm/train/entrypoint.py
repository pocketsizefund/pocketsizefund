import argparse
import os
import pickle

from keras import models, layers, losses, optimizers, metrics

from pkg.storage import storage
from pkg.features import features


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

features_count = 1

output_length = features.WINDOW_OUTPUT_LENGTH

storage_client = storage.Client(
    s3_data_bucket_name=arguments.s3_data_bucket_name,
)

features_client = features.Client()

file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_FEATURES_PATH,
)

max_version_file_name = storage_client.get_max_prefix_version(
    prefixes=file_names,
)

features_by_version = storage_client.load_dataframes(
    prefix=storage.PREFIX_FEATURES_PATH,
    file_names=[max_version_file_name],
)

training_features = features_by_version[max_version_file_name]

available_tickers = arguments.available_tickers.split(',')

filtered_features = training_features[training_features['ticker'].isin(
    available_tickers
)]

most_recent_filtered_features = filtered_features.groupby('ticker').apply(
    lambda group: group.nlargest(arguments.days, 'timestamp')
).reset_index(drop=True)

preprocessed_features = features_client.preprocess_training_features(
    data=most_recent_filtered_features,
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
    x=preprocessed_features['data']['training'],
    epochs=arguments.epochs,
    validation_data=preprocessed_features['data']['validating'],
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
    obj=preprocessed_features['scalers'],
    file=scalers_file,
)

preprocessed_features['data']['testing'].save(
    path=os.path.join(arguments.model_dir, 'testing_data'),
    compression='GZIP',
)
