import os

from pkg.trade import trade


def handler(event: any, context: any) -> dict[str, any]:
    trade_client = trade.Client(
        darqube_api_key=os.getenv('DARQUBE_API_KEY'),
        alpaca_api_key=os.getenv('ALPACA_API_KEY'),
        alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
        alpaca_account_id=os.getenv('ALPACA_ACCOUNT_ID'),
        is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
    )

    trade_client.clear_positions()
