import os
import datetime

import pandas

from pkg.storage import storage
from pkg.trade import trade
from pkg.data import data


def handler(event: any, context: any) -> dict[str, any]:
    storage_client = storage.Client(
        s3_data_bucket=os.getenv('S3_DATA_BUCKET_NAME')
    )

    trade_client = trade.Client(
        finnhub_api_key=os.getenv('FINNHUB_API_KEY'),
        alpaca_api_key_id=os.getenv('ALPACA_API_KEY_ID'),
        alpaca_api_secret_key=os.getenv('ALPACA_API_SECRET_KEY'),
        alpaca_account_id=os.getenv('ALPACA_ACCOUNT_ID'),
        is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
    )

    data_client = data.Client(
        alpha_vantage_api_key=os.getenv('ALPHA_VANTAGE_API_KEY'),
        alpaca_api_key=os.getenv('ALPACA_API_KEY'),
        alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
    )

    file_names = storage_client.list_file_names(
        prefix=storage.PREFIX_EQUITY_BARS_PATH,
    )

    file_names.sort(reverse=True)

    most_recent_file_name = file_names[0]

    all_old_bars = storage_client.load_dataframes(
        prefix=storage.PREFIX_EQUITY_BARS_PATH,
        file_names=[most_recent_file_name],
    )

    most_recent_bars = all_old_bars[most_recent_file_name]

    start_at = most_recent_bars['timestamp'].max()

    end_at = datetime.datetime.today()

    tickers = trade_client.get_available_tickers()

    new_bars = data_client.get_equity_bars(
        tickers=tickers,
        start_at=start_at,
        end_at=end_at,
    )

    new_bars_grouped_by_year = new_bars.groupby(
        new_bars.timestamp.dt.year,
    )

    all_updated_bars: dict[str, pandas.DataFrame] = []
    for year in new_bars_grouped_by_year.groups:
        new_bars = new_bars_grouped_by_year.get_group(year)

        if year in all_old_bars:
            old_bars = all_old_bars[year]

            updated_bars = pandas.concat(
                objs=[old_bars, new_bars],
                ignore_index=True,
            )
            updated_bars.drop_duplicates(inplace=True)

            all_updated_bars[year] = updated_bars

        else:
            all_updated_bars[year] = new_bars

    storage_client.save_dataframes(
        prefix=storage.PREFIX_EQUITY_BARS_PATH,
        dataframes=all_updated_bars,
    )
