import os
import datetime

import pandas

from pkg.storage import storage
from pkg.trade import trade
from pkg.data import data
from pkg.message import message


storage_client = storage.Client(
    s3_data_bucket_name=os.getenv('S3_DATA_BUCKET_NAME')
)

trade_client = trade.Client(
    darqube_api_key=os.getenv('DARQUBE_API_KEY'),
    alpaca_api_key=os.getenv('ALPACA_API_KEY'),
    alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
    is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
)

data_client = data.Client(
    alpaca_api_key=os.getenv('ALPACA_API_KEY'),
    alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
)

message_client = message.Client(
    topic_arn=os.getenv('SNS_ERRORS_TOPIC_ARN'),
)


def handler(event: any, context: any) -> dict[str, any]:
    try:
        file_names = storage_client.list_file_names(
            prefix=storage.PREFIX_EQUITY_BARS_PATH,
        )

        file_names.sort(reverse=True)

        first_stored_file_name = file_names[0]
        second_stored_file_name = file_names[1]

        old_bars_by_year = storage_client.load_dataframes(
            prefix=storage.PREFIX_EQUITY_BARS_PATH,
            file_names=[
                first_stored_file_name,
                second_stored_file_name,
            ]
        )

        start_at = old_bars_by_year[first_stored_file_name]['timestamp'].max()

        end_at = datetime.datetime.today()

        tickers = trade_client.get_available_tickers()

        new_bars = data_client.get_range_equities_bars(
            tickers=tickers,
            start_at=start_at,
            end_at=end_at,
        )

        if new_bars.empty:
            raise Exception('no new bars')

        new_bars_grouped_by_year = new_bars.groupby(
            new_bars.timestamp.dt.year,
        )

        updated_bars_by_year: dict[str, pandas.DataFrame] = {}
        for year in new_bars_grouped_by_year.groups:
            year_string = str(year)

            new_bars_group = new_bars_grouped_by_year.get_group(year)

            if year_string in old_bars_by_year.keys():
                old_bars = old_bars_by_year[year_string]

                updated_bars = pandas.concat(
                    objs=[old_bars, new_bars_group],
                    ignore_index=True,
                )
                updated_bars.drop_duplicates(
                    subset=['timestamp', 'ticker'],
                    inplace=True,
                )

                updated_bars_by_year[year_string] = updated_bars

            else:
                updated_bars_by_year[year_string] = new_bars_group

        storage_client.store_dataframes(
            prefix=storage.PREFIX_EQUITY_BARS_PATH,
            dataframes=updated_bars_by_year,
        )

    except Exception as error:
        message_client.send_message(
            subject='Fetch data Lambda errored',
            message='Error: {}'.format(error),
        )

        raise error
