"""Generate features for equity bars data from raw data in S3.

This script assumes that the raw data has been downloaded to the
current working directory. It generates features and uploads them to S3.
"""
import argparse

from pkg.config import config
from pkg.features import features
from pkg.storage import storage

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
    s3_artifacts_bucket_name=samconfig_file.get_parameter(
        "S3ArtifactsBucketName",
    ),
)

features_client = features.Client()

equity_bars_by_file_name = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_RAW_PATH,
    file_names=["all.csv"],
)

equity_bars = equity_bars_by_file_name["all.csv"]

generated_features = features_client.generate_features(
    data=equity_bars,
)

null_values_check = generated_features.isna().any().any()

if null_values_check:
    msg = "generated features contains null values"
    raise ValueError(msg)

storage_client.store_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_FEATURES_PATH,
    dataframes_by_file_name={"all.csv": generated_features},
)
