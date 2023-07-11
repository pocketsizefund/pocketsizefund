import pandas

from pkg.config import config
from pkg.storage import storage
from pkg.model import model


samconfig_file = config.SAMConfig(
    'samconfig.toml',
    config.ENVIRONMENT_DEVELOPMENT,
)

storage_client = storage.Client(
    s3_data_bucket_name=samconfig_file.get_parameter('S3DataBucketName'),
)

file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
)

equity_bars_dataframes_by_year = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
    file_names=file_names,
)

equity_bars_dataframe = pandas.concat(
    list(equity_bars_dataframes_by_year.values()),
)

model_client = model.Client()

model_client.train_model(equity_bars_dataframe)

model_client.save_model(file_path='lstm_model.h5')
