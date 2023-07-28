import os

import pandas

from pkg.storage import storage
from pkg.trade import trade
from pkg.model import model
from pkg.message import message


POSITIONS_COUNT = 10

storage_client = storage.Client(
    s3_data_bucket_name=os.environ['S3_DATA_BUCKET_NAME'],
)

trade_client = trade.Client(
    darqube_api_key=os.getenv('DARQUBE_API_KEY'),
    alpaca_api_key=os.getenv('ALPACA_API_KEY'),
    alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
    is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
)

model_client = model.Client(
    file_path=os.getenv('MODEL_FILE_PATH'),
)

message_client = message.Client(
    topic_arn=os.getenv('SNS_ERRORS_TOPIC_ARN'),
)


def handler(event: any, context: any) -> dict[str, any]:
    try:
        market_status = trade_client.get_market_status()
        if not market_status['is_market_open']:
            raise Exception('market is closed')

        file_names = storage_client.list_file_names(
            prefix=storage.PREFIX_EQUITY_BARS_PATH,
        )

        old_equity_bars_by_year = storage_client.load_dataframes(
            prefix=storage.PREFIX_EQUITY_BARS_PATH,
            file_names=file_names,
        )

        old_bars = pandas.concat(old_equity_bars_by_year.values())

        old_bars_grouped_by_ticker = old_bars.groupby('ticker')

        available_tickers = trade_client.get_available_tickers()

        prediction_data = old_bars_grouped_by_ticker.head(
            model.DAYS_TO_TRAIN,
        )

        prediction_data_filtered = prediction_data[
            prediction_data['ticker'].isin(available_tickers)
        ]

        predictions_by_ticker = model_client.get_model_predictions(
            data=prediction_data_filtered,
        )

        del prediction_data
        del prediction_data_filtered

        current_prices_by_ticker = trade_client.get_current_prices(
            tickers=list(predictions_by_ticker.keys()),
        )

        moves_by_ticker = {
            ticker: predictions_by_ticker[ticker]['prices'][0] -
            current_prices_by_ticker[ticker]['price']
            for ticker in predictions_by_ticker
        }

        sorted_moves_by_ticker = dict(sorted(
            moves_by_ticker.items(),
            key=lambda item: item[1], reverse=True,
        ))

        del moves_by_ticker

        highest_moves_by_ticker = {
            k: sorted_moves_by_ticker[k]
            for k in list(sorted_moves_by_ticker)[:POSITIONS_COUNT]
        }

        highest_moves_tickers = highest_moves_by_ticker.keys()

        weights_by_ticker = {
            ticker: 1 / POSITIONS_COUNT
            for ticker in highest_moves_tickers
        }

        available_cash = trade_client.get_available_cash()

        positions: list[dict[str, any]] = [
            {
                'ticker': ticker_weight[0],
                'quantity': (available_cash * ticker_weight[1]) /
                current_prices_by_ticker[ticker_weight[0]]['price'],
                'side': trade.SIDE_BUY,
            }
            for ticker_weight in weights_by_ticker.items()
        ]

        trade_client.set_positions(positions)

    except Exception as error:
        message_client.send_message(
            subject='Create positions Lambda errored',
            message='Error: {}'.format(error),
        )

        raise error
