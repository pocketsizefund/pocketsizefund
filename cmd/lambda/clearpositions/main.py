import os

from pkg.trade import trade
from pkg.message import message


trade_client = trade.Client(
    darqube_api_key=os.getenv('DARQUBE_API_KEY'),
    alpaca_api_key=os.getenv('ALPACA_API_KEY'),
    alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
    is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
)

message_client = message.Client(
    topic_arn=os.getenv('SNS_ERRORS_TOPIC_ARN'),
)


def handler(event: any, context: any) -> dict[str, any]:
    try:
        market_status = trade_client.get_market_status()
        if not market_status['is_market_open']:
            raise Exception('market is closed')

        trade_client.clear_positions()

    except Exception as error:
        message_client.send_message(
            subject='Clear positions Lambda errored',
            message='Error: {}'.format(error),
        )

        raise error
