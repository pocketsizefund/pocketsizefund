import os

from pkg.trade import trade


def handler(event: any, context: any) -> dict[str, any]:
    trade_client = trade.Client(
        finnhub_api_key=os.getenv('FINNHUB_API_KEY'),
        alpaca_api_key_id=os.getenv('ALPACA_API_KEY_ID'),
        alpaca_api_secret_key=os.getenv('ALPACA_API_SECRET_KEY'),
        alpaca_account_id=os.getenv('ALPACA_ACCOUNT_ID'),
        is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
    )

    trade_client.clear_positions()
