"""Download all raw data from S3."""

from pocketsizefund.config import config
from pocketsizefund.storage import storage

config_file = config.Config(
    environment=config.ENVIRONMENT_DEVELOPMENT,
)

storage_client = storage.Client(
    s3_data_bucket_name=config_file.get_parameter("s3_data_bucket_name"),
    s3_artifacts_bucket_name=config_file.get_parameter("s3_artifacts_bucket_name"),
)

file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_RAW_PATH,
)

file_names = sorted(file_names, reverse=True)

file_name = file_names[0]

equity_bars_raw_data_by_file_name = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_RAW_PATH,
    file_names=[file_name],
)

equity_bars_raw_data_by = equity_bars_raw_data_by_file_name[file_name]

equity_bars_raw_data_by.to_csv(file_name, index=False)
