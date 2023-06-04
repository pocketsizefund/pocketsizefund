import pandas

from pkg.storage import storage
from pkg.config import config


samconfig_file = config.SAMConfig('samconfig.toml')

storage_client = storage.Client(
    s3_data_bucket_name=samconfig_file.get_parameter('S3DataBucketName'),
)

file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
)

bars_by_year = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
    file_names=file_names,
)

bars = pandas.concat(bars_by_year.values())

bars.to_csv(
    path_or_buf='bars.csv',
    index=False,
    header=True,
)
