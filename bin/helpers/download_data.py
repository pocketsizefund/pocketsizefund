"""Download all raw data from S3."""  # noqa: INP001
from pkg.config import config
from pkg.storage import storage

samconfig_file = config.SAMConfig(
    file_path="samconfig.toml",
    environment=config.ENVIRONMENT_DEVELOPMENT,
)

storage_client = storage.Client(
    s3_data_bucket_name=samconfig_file.get_parameter("S3DataBucketName"),
    s3_artifacts_bucket_name=samconfig_file.get_parameter("S3ArtifactsBucketName"),
)

equity_bars_raw_data_by_file_name = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_RAW_PATH,
    file_names=["all.csv"],
)

equity_bars_raw_data_by = equity_bars_raw_data_by_file_name["all.csv"]

equity_bars_raw_data_by.to_csv("equity_bars_raw_data.csv", index=False)

