import argparse

from pkg.model import model
from pkg.storage import storage


parser = argparse.ArgumentParser()

parser.add_argument(
    '--s3-data-bucket-name',
    type=str,
    dest='s3_data_bucket_name',
)

parser.add_argument(
    '--s3-artifacts-bucket-name',
    type=str,
    dest='s3_artifacts_bucket_name',
)

parser.add_argument(
    '--model_dir',  # underscore required for SageMaker
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

parser.add_argument(
    '--notes',
    type=str,
    dest='notes',
)

arguments = parser.parse_args()

secrets_file = open('secrets.env', 'r')
secrets_content = secrets_file.read()
secrets_file.close()
weights_and_biases_api_key = secrets_content.split('=')[1].strip()

storage_client = storage.Client(
    s3_data_bucket_name=arguments.s3_data_bucket_name,
    s3_artifacts_bucket_name=arguments.s3_artifacts_bucket_name,
)

model_model = model.Model(
    artifact_output_path=arguments.model_dir,
    weights_and_biases_api_key=weights_and_biases_api_key,
    notes=arguments.notes,
)

features_by_file_name = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_FEATURES_PATH,
    file_names=['all.csv'],
)

training_features = features_by_file_name['all.csv']

available_tickers = arguments.available_tickers.split(',')

filtered_features = training_features[training_features['ticker'].isin(
    available_tickers
)]

most_recent_filtered_features = filtered_features.groupby('ticker').apply(
    lambda group: group.nlargest(arguments.days, 'timestamp')
).reset_index(drop=True)

preprocessed_features = model_model.preprocess_training_features(
    data=most_recent_filtered_features,
)

training_output = model_model.train_model(
    features=preprocessed_features['data'],
    epochs=arguments.epochs,
)

model_model.save_model(
    model=training_output['model'],
)

model_model.save_metrics(
    metrics=training_output['metrics'],
)

model_model.save_scalers(
    scalers=preprocessed_features['scalers'],
)

model_model.save_data(
    name='testing_data',
    data=preprocessed_features['data']['testing'],
)
