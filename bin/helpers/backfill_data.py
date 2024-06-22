"""Backfill data to S3."""

import argparse
import datetime

from pocketsizefund.config import config
from pocketsizefund.data import data
from pocketsizefund.storage import storage
from pocketsizefund.trade import trade

parser = argparse.ArgumentParser(
    prog="backfill data helper script",
    description="update s3 data bucket with training data",
)

parser.add_argument(
    "--config-file-path",
    type=str,
    required=True,
    dest="config_file_path",
)

arguments = parser.parse_args()

config_file = config.Config(
    environment=config.ENVIRONMENT_DEVELOPMENT,
)

storage_client = storage.Client(
    s3_data_bucket_name=config_file.get_parameter("s3_data_bucket_name"),
    s3_artifacts_bucket_name=config_file.get_parameter(
        "s3_artifacts_bucket_name",
    ),
)

data_client = data.Client(
    alpaca_api_key=config_file.get_parameter("alpaca_api_key"),
    alpaca_api_secret=config_file.get_parameter("alpaca_api_secret"),
    edgar_user_agent=config_file.get_parameter("edgar_user_agent"),
    print_logs=True,
)

trade_client = trade.Client(
    darqube_api_key=config_file.get_parameter("darqube_api_key"),
    alpaca_api_key=config_file.get_parameter("alpaca_api_key"),
    alpaca_api_secret=config_file.get_parameter("alpaca_api_secret"),
    alpha_vantage_api_key=config_file.get_parameter("alpha_vantage_api_key"),
    is_paper=True,
)

available_tickers: list[str] = trade_client.get_available_tickers()

print("tickers count: ", len(available_tickers))  # noqa: T201

end_at = datetime.datetime.now(tz=config.TIMEZONE)
start_at = end_at - datetime.timedelta(days=365 * 7)

equity_raw_data = data_client.get_range_equities_bars(
    tickers=available_tickers,
    start_at=start_at,
    end_at=end_at,
)

null_values_check = equity_raw_data.isna().any().any()

if null_values_check:
    msg = "data contains null values"
    raise Exception(msg)  # noqa: TRY002

file_name_prefix = end_at.strftime("%Y-%m-%d-%H-%M-%S")

storage_client.store_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_RAW_PATH,
    dataframes_by_file_name={f"{file_name_prefix}-all.csv": equity_raw_data},
)

print("backfill data complete")  # noqa: T201
