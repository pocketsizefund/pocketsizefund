import os
import datetime

from pkg.storage import storage
from pkg.model import model
from pkg.trade import trade


def handler(event: any, context: any) -> dict[str, any]:
    storage_client = storage.Client(
        s3_data_bucket_name=os.environ['S3_DATA_BUCKET_NAME'],
    )

    file_name = str(datetime.datetime.now().year)

    file_names = storage_client.list_file_names(
        prefix=storage.PREFIX_EQUITY_BARS_PATH,
    )

    file_names.sort(reverse=True)

    if file_name not in file_names:
        file_name = file_names[0]

    old_dataframes = storage_client.load_dataframes(
        prefix=storage.PREFIX_EQUITY_BARS_PATH,
        file_names=[file_name],
    )

    old_dataframe = old_dataframes[file_name]

    old_dataframe_grouped_by_ticker = old_dataframe.groupby('ticker')

    if old_dataframe_grouped_by_ticker.size() < model.DAYS_TO_TRAIN:
        file_name = file_names[1]

        old_dataframes = storage_client.load_dataframes(
            prefix=storage.PREFIX_EQUITY_BARS_PATH,
            file_names=[file_name],
        )

        old_dataframe = old_dataframes[file_name]

    prediction_data = old_dataframe.head(model.DAYS_TO_TRAIN)

    model_client = model.Client()

    model_client.load_model(os.getenv('MODEL_NAME'))

    trade_client = trade.Client(
        finnhub_api_key=os.getenv('FINNHUB_API_KEY'),
        alpaca_api_key_id=os.getenv('ALPACA_API_KEY_ID'),
        alpaca_api_secret_key=os.getenv('ALPACA_API_SECRET_KEY'),
        alpaca_account_id=os.getenv('ALPACA_ACCOUNT_ID'),
        is_paper=os.getenv('IS_PAPER'),
    )

    available_tickers = trade_client.get_available_tickers()

    predictions_by_ticker = model_client.get_model_predictions(prediction_data)

    predictions_by_ticker = predictions_by_ticker[predictions_by_ticker['ticker'].isin(
        available_tickers),
    ]

    current_prices_by_ticker = trade_client.get_current_prices(
        tickers=predictions_by_ticker['ticker'].tolist(),
    )

    buying_power = trade_client.get_buying_power()

    positions: list[dict[str, any]] = []
    for ticker, price_predictions in predictions_by_ticker.iterrows():
        current_price = current_prices_by_ticker[ticker]
        next_price = price_predictions[0]

        positions.append({
            'ticker': ticker,
            'quantity': (buying_power / len(predictions_by_ticker)) / current_price,
            'side': trade.SIDE_BUY if next_price > current_price else trade.SIDE_SELL,
        })

    trade_client.set_positions(positions)
