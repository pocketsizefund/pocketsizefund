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
    log_progress=True,
)

trade_client = trade.Client(
    finnhub_api_key=samconfig_file.get_parameter('FinnhubAPIKey'),
    alpaca_api_key=samconfig_file.get_parameter('AlpacaPaperAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaPaperAPISecret'),
    is_paper=True,
)

tickers = trade_client.get_available_tickers()

equities_raw_bars = data_client.get_equities_raw_bars(
    tickers=tickers,
)

dataframes_by_ticker: list[pandas.DataFrame] = []
for equity_raw_bars in equities_raw_bars:
    dataframe = data_client.convert_equity_raw_bars_to_dataframe(
        equity_raw_bars=equity_raw_bars,
    )

    dataframes_by_ticker.append(dataframe)

dataframe = pandas.concat(dataframes_by_ticker)

dataframe.drop_duplicates(subset=['timestamp', 'ticker'], inplace=True)

grouped_dataframe = dataframe.groupby(dataframe.timestamp.dt.year)

dataframes_by_year: dict[str, pandas.DataFrame] = {}
for group_name in grouped_dataframe.groups.keys():
    group = grouped_dataframe.get_group(group_name)
    group = group.drop_duplicates()

    group_year_file_name = '{}.csv.gz'.format(str(group_name))

    dataframes_by_year[group_year_file_name] = group

storage_client.store_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_PATH,
    dataframes=dataframes_by_year,
)
