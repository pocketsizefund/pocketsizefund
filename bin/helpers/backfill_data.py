import argparse
import datetime

from pkg.config import config
from pkg.data import data
from pkg.storage import storage
from pkg.trade import trade

parser = argparse.ArgumentParser(
    prog='backfill data helper script',
    description='update s3 data bucket with training data',
)

parser.add_argument(
    '--samconfig-file-path',
    type=str,
    required=True,
    dest='samconfig_file_path',
)

arguments = parser.parse_args()

samconfig_file = config.SAMConfig(
    file_path=arguments.samconfig_file_path,
    environment=config.ENVIRONMENT_DEVELOPMENT,
)

storage_client = storage.Client(
    s3_data_bucket_name=samconfig_file.get_parameter('S3DataBucketName'),
    s3_artifacts_bucket_name=samconfig_file.get_parameter(
        'S3ArtifactsBucketName'
    ),
)

data_client = data.Client(
    alpaca_api_key=samconfig_file.get_parameter('AlpacaAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaAPISecret'),
    edgar_user_agent=samconfig_file.get_parameter('EDGARUserAgent'),
    print_logs=True,
)

trade_client = trade.Client(
    darqube_api_key=samconfig_file.get_parameter('DarqubeAPIKey'),
    alpaca_api_key=samconfig_file.get_parameter('AlpacaAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaAPISecret'),
    alpha_vantage_api_key=samconfig_file.get_parameter('AlphaVantageAPIKey'),
    is_paper=True,
)

available_tickers: list[str] = trade_client.get_available_tickers()

print('tickers count: ', len(available_tickers))

full_end_at = datetime.datetime.today()
full_start_at = full_end_at - datetime.timedelta(days=365 * 7)

equity_raw_data = data_client.get_range_equities_bars(
    tickers=available_tickers,
    start_at=full_start_at,
    end_at=full_end_at,
)

null_values_check = equity_raw_data.isnull().any().any()

if null_values_check:
    raise Exception('equity raw data contains null values')

storage_client.store_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_RAW_PATH,
    dataframes_by_file_name={'all.csv': equity_raw_data},
)

print('backfill data complete')
