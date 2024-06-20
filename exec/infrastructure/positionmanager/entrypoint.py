"""Set positions based on portfolio position and model predictions."""

import datetime
import os

import requests
from loguru import logger
from pocketsizefund.config import config
from pocketsizefund.trade import trade

STATUS_CODE_OK = 200
POSITIONS_COUNT = 10


def get_predictions() -> dict[str, any]:
    """Set positions based on portfolio position and model predictions."""
    trade_client = trade.Client(
        darqube_api_key=os.getenv("DARQUBE_API_KEY"),
        alpaca_api_key=os.getenv("ALPACA_API_KEY"),
        alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
        alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
        is_paper=os.getenv("IS_PAPER") == "true",
    )

    now = datetime.datetime.now(tz=config.TIMEZONE)

    is_clear = trade_client.check_set_position_availability(
        action=trade.CLEAR_ACTION,
        current_datetime=now,
    )

    is_create = trade_client.check_set_position_availability(
        action=trade.CREATE_ACTION,
        current_datetime=now,
    )

    if is_clear:
        trade_client.clear_positions()

    if is_create:
        response = requests.get(
            url="http://price-model:8080/predictions",
            timeout=30,
        )

        if response.status_code != STATUS_CODE_OK:
            msg = f"error getting predictions: {response.text}"
            raise Exception(msg)  # noqa: TRY002

        predictions_by_ticker = response.json()

        moves_by_ticker = {
            ticker: predictions_by_ticker[ticker][0] - predictions_by_ticker[ticker][4]
            for ticker in predictions_by_ticker
        }

        sorted_moves_by_ticker = dict(
            sorted(
                moves_by_ticker.items(),
                key=lambda item: item[1],
                reverse=True,
            ),
        )

        highest_moves_by_ticker = {
            k: sorted_moves_by_ticker[k] for k in list(sorted_moves_by_ticker)[:POSITIONS_COUNT]
        }

        highest_moves_tickers = highest_moves_by_ticker.keys()

        if len(highest_moves_tickers) == 0:
            msg = "no tickers to trade"
            raise Exception(msg)  # noqa: TRY002

        trade_client.set_positions(tickers=highest_moves_tickers)

    return None


if __name__ == "__main__":
    try:
        get_predictions()
    except ValueError as e:
        logger.debug(f"error getting predictions: {e}")
