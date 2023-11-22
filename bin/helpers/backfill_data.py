import argparse
import datetime

import pandas

from pkg.config import config
from pkg.storage import storage
from pkg.data import data
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
)

data_client = data.Client(
    alpaca_api_key=samconfig_file.get_parameter('AlpacaAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaAPISecret'),
    print_logs=True,
)

trade_client = trade.Client(
    darqube_api_key=samconfig_file.get_parameter('DarqubeAPIKey'),
    alpaca_api_key=samconfig_file.get_parameter('AlpacaAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaAPISecret'),
    is_paper=True,
)

available_tickers: list[str] = trade_client.get_available_tickers()

print('tickers count: ', len(available_tickers))

file_names: list[str] = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
)

bars: pandas.DataFrame = None

full_end_at = datetime.datetime.today()
full_start_at = full_end_at - datetime.timedelta(days=365 * 7)

if len(file_names) == 0:
    print('backfill all data')

    bars = data_client.get_range_equities_bars(
        tickers=available_tickers,
        start_at=full_start_at,
        end_at=full_end_at,
    )

else:
    print('backfill update data')

    old_bars_by_year: dict[int, pandas.DataFrame] = storage_client.load_dataframes(
        prefix=storage.PREFIX_EQUITY_BARS_PATH,
        file_names=file_names,
    )

    old_bars: pandas.DataFrame = pandas.concat(old_bars_by_year.values())

    start_at: datetime.datetime = old_bars.groupby(
        by='ticker',
    )['timestamp'].max().min().to_pydatetime()

    end_at: datetime.datetime = datetime.datetime.today()

    old_tickers: list[str] = old_bars['ticker'].unique().tolist()

    update_tickers: list[str] = list(set(available_tickers) & set(old_tickers))

    update_bars: pandas.DataFrame = data_client.get_range_equities_bars(
        tickers=update_tickers,
        start_at=start_at,
        end_at=end_at,
    )

    new_tickers: list[str] = list(set(available_tickers) - set(old_tickers))

    new_bars: pandas.DataFrame = data_client.get_range_equities_bars(
        tickers=new_tickers,
        start_at=full_start_at,
        end_at=full_end_at,
    )

    bars: pandas.DataFrame = pandas.concat([
        old_bars,
        update_bars,
        new_bars,
    ]).drop_duplicates(
        subset=[
            'ticker',
            'timestamp',
        ],
    )

bars_grouped_by_year: pandas.DataFrameGroupBy[int] = bars.groupby(
    bars.timestamp.dt.year
)

bars_by_year: dict[str, pandas.DataFrame] = {}

for group_name in bars_grouped_by_year.groups.keys():
    group: pandas.DataFrame = bars_grouped_by_year.get_group(group_name)
    group = group.drop_duplicates()

    group_year: str = str(group_name)

    bars_by_year[group_year] = group

storage_client.store_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
    dataframes=bars_by_year,
)

print('backfill data complete')
