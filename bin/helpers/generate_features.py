import argparse

import pandas

from pkg.config import config
from pkg.storage import storage
from pkg.features import features


parser = argparse.ArgumentParser(
    prog="backfill data helper script",
    description="update s3 data bucket with training data",
)

parser.add_argument(
    "--samconfig-file-path",
    type=str,
    required=True,
    dest="samconfig_file_path",
)

arguments = parser.parse_args()

samconfig_file = config.SAMConfig(
    file_path=arguments.samconfig_file_path,
    environment=config.ENVIRONMENT_DEVELOPMENT,
)

storage_client = storage.Client(
    s3_data_bucket_name=samconfig_file.get_parameter("S3DataBucketName"),
)

features_client = features.Client()

equity_bar_file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
)

equity_bars_by_year = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
    file_names=equity_bar_file_names,
)

equity_bars = pandas.concat([equity_bars_by_year[year] for year in equity_bars_by_year])

generated_features = features_client.generate_features(
    data=equity_bars,
)

null_values_check = generated_features.isnull().any().any()

if null_values_check:
    raise Exception("generated features contains null values")


generated_feature_file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_FEATURES_PATH,
)

next_version = storage_client.get_next_prefix_version(
    prefixes=generated_feature_file_names,
)

storage_client.store_dataframes(
    prefix=storage.PREFIX_FEATURES_PATH,
    dataframes={
        next_version: generated_features,
    },
)
