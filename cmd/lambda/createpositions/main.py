import os
import heapq

import pandas

from pkg.storage import storage
from pkg.trade import trade
from pkg.model import model
from pkg.portfolio import portfolio


POSITIONS_COUNT = 10
BUYING_POWER_PERCENTAGE = 0.95  # preventing overspend


def handler(event: any, context: any) -> dict[str, any]:
    storage_client = storage.Client(
        s3_data_bucket_name=os.environ['S3_DATA_BUCKET_NAME'],
    )

    trade_client = trade.Client(
        darqube_api_key=os.getenv('DARQUBE_API_KEY'),
        alpaca_api_key=os.getenv('ALPACA_API_KEY'),
        alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
        alpaca_account_id=os.getenv('ALPACA_ACCOUNT_ID'),
        is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
    )

    model_client = model.Client(
        file_path=os.getenv('MODEL_FILE_PATH'),
    )

    portfolio_client = portfolio.Client()

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
        prediction_data['ticker'].isin(available_tickers),
    ]

    predictions_by_ticker = model_client.get_model_predictions(
        data=prediction_data_filtered,
    )

    current_prices_by_ticker = trade_client.get_current_prices(
        tickers=list(predictions_by_ticker.keys()),
    )

    moves_by_ticker = {
        ticker: predictions_by_ticker[ticker][0] -
        current_prices_by_ticker[ticker]
        for ticker in predictions_by_ticker
    }

    highest_moves_by_ticker = heapq.nlargest(
        n=POSITIONS_COUNT,
        iterable=moves_by_ticker.items(),
        key=lambda item: item[1],
    )

    highest_moves_tickers = highest_moves_by_ticker.keys()

    portfolio_data = old_bars_grouped_by_ticker.filter(
        lambda x: x['ticker'].iloc[0] in highest_moves_tickers,
    )

    weights_by_ticker = portfolio_client.calculate_weights(
        data=portfolio_data,
    )

    buying_power = trade_client.get_buying_power() * BUYING_POWER_PERCENTAGE

    positions: list[dict[str, any]] = [
        {
            'ticker': ticker,
            'quantity': (buying_power * weight) / current_prices_by_ticker[ticker],
            'side': trade.SIDE_BUY,
        }
        for ticker, weight in weights_by_ticker.items()
    ]

    trade_client.set_positions(positions)
