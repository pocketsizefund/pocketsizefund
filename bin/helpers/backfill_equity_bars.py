import datetime

import pandas

from pkg.config import config
from pkg.storage import storage
from pkg.data import data
from pkg.trade import trade


samconfig_file = config.SAMConfig('samconfig.toml')

storage_client = storage.Client(
    s3_data_bucket_name=samconfig_file.get_parameter('S3DataBucketName'),
)

data_client = data.Client(
    alpha_vantage_api_key=samconfig_file.get_parameter('AlphaVantageAPIKey'),
    alpaca_api_key=samconfig_file.get_parameter('AlpacaPaperAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaPaperAPISecret'),
    print_logs=True,
)

trade_client = trade.Client(
    finnhub_api_key=samconfig_file.get_parameter('FinnhubAPIKey'),
    alpaca_api_key=samconfig_file.get_parameter('AlpacaPaperAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaPaperAPISecret'),
    is_paper=True,
)

available_tickers = trade_client.get_available_tickers()

file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
)

bars: pandas.DataFrame = None
if len(file_names) == 0:
    bars = data_client.get_all_equities_bars(
        tickers=available_tickers,
    )

else:
    bars_by_year = storage_client.load_dataframes(
        prefix=storage.PREFIX_EQUITY_BARS_PATH,
        file_names=file_names,
    )

    old_bars = pandas.concat(bars_by_year.values())

    ticker_grouped_bars = old_bars.groupby('ticker')
    most_recent_timestamps = ticker_grouped_bars['timestamp'].max()

    newest_timestamps = pandas.DataFrame({
        'ticker': most_recent_timestamps.index,
        'timestamp': most_recent_timestamps.values
    })

    new_tickers = list(
        set(available_tickers) - set(newest_timestamps['ticker']),
    )
    tickers_in_rows = newest_timestamps[newest_timestamps['ticker'].isin(
        available_tickers,
    )]
    rows_not_in_tickers = newest_timestamps[~newest_timestamps['ticker'].isin(
        available_tickers,
    )]

    start_at = min(
        tickers_in_rows['timestamp'].min(),
        rows_not_in_tickers['timestamp'].min(),
    )

    end_at = datetime.datetime.today()

    update_tickers = tickers_in_rows['ticker'].tolist() \
        + rows_not_in_tickers['ticker'].tolist()

    new_bars = data_client.get_all_equities_bars(
        tickers=new_tickers,
    )

    updated_bars = data_client.get_range_equities_bars(
        tickers=update_tickers,
        start_at=start_at,
        end_at=end_at,
    )

    bars = pandas.concat(objs=[
        old_bars,
        new_bars,
        updated_bars,
    ]).drop_duplicates(
        subset=[
            'tickers',
            'timestamp',
        ],
    )

year_grouped_bars = bars.groupby(bars.timestamp.dt.year)

bars_by_year: dict[str, pandas.DataFrame] = {}

for group_name in year_grouped_bars.groups.keys():
    group = year_grouped_bars.get_group(group_name)
    group = group.drop_duplicates()

    group_year = str(group_name)

    bars_by_year[group_year] = group

storage_client.store_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
    dataframes=bars_by_year,
)
