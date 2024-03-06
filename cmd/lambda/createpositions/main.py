import os
import datetime

from pkg.storage import storage
from pkg.trade import trade
from pkg.data import data
from pkg.model import model


POSITIONS_COUNT = 10

storage_client = storage.Client(
    s3_data_bucket_name=os.environ['S3_DATA_BUCKET_NAME'],
    s3_artifacts_bucket_name=os.environ['S3_ARTIFACTS_BUCKET_NAME'],
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
    print_logs=True,
)

model_client = model.Client(
    model_endpoint_name=os.getenv('MODEL_ENDPOINT_NAME'),
)


def handler(
    event: any,
    context: any,
) -> dict[str, any]:
    _ = event, context

    if not trade_client.is_market_open():
        raise Exception('market is closed')

    available_tickers = trade_client.get_available_tickers()

    end_at = datetime.datetime.now()
    start_at = end_at - datetime.timedelta(days=50)

    prediction_data = data_client.get_range_equities_bars(
        tickers=available_tickers,
        start_at=start_at,
        end_at=end_at,
    )

    prediction_data = prediction_data.sort_values(
        by='timestamp',
        ascending=False,
    ).groupby('ticker').head(30).reset_index(drop=True)

    prediction_data['timestamp'] = prediction_data['timestamp'].astype(str)

    predictions_by_ticker = model_client.generate_predictions(
        data=prediction_data,
    )

    moves_by_ticker = {
        ticker: predictions_by_ticker[ticker][0][0] -
        predictions_by_ticker[ticker][4][0]
        for ticker in predictions_by_ticker
    }

    sorted_moves_by_ticker = dict(sorted(
        moves_by_ticker.items(),
        key=lambda item: item[1], reverse=True,
    ))

    highest_moves_by_ticker = {
        k: sorted_moves_by_ticker[k]
        for k in list(sorted_moves_by_ticker)[:POSITIONS_COUNT]
    }

    highest_moves_tickers = highest_moves_by_ticker.keys()

    if len(highest_moves_tickers) == 0:
        raise Exception('no tickers to trade')

    trade_client.set_positions(tickers=highest_moves_tickers)
