import os
import datetime

from pkg.trade import trade
from pkg.config import config


trade_client = trade.Client(
    darqube_api_key=os.getenv("DARQUBE_API_KEY"),
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
    alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
    is_paper=True if os.getenv("IS_PAPER") == "true" else False,
)


def handler(
    event: any,
    context: any,
) -> dict[str, any]:
    _ = event, context

    if not trade_client.check_set_position_availability(
        action=trade.CLEAR_ACTION,
        current_datetime=datetime.datetime.now(tz=config.TIMEZONE),
    ):
        return

    trade_client.clear_positions()
