import os

from pkg.trade import trade


def handler(event: any, context: any) -> dict[str, any]:
    trade_client = trade.Client(
        darqube_api_key=os.getenv('DARQUBE_API_KEY'),
        alpaca_api_key=os.getenv('ALPACA_API_KEY'),
        alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
        is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
    )

    market_status = trade_client.get_market_status()
    if not market_status['is_market_open']:
        raise Exception('market is closed')

    trade_client.clear_positions()
