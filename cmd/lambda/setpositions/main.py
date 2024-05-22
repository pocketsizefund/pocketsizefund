"""Set positions based on portfolio position and model predictions."""

import datetime
import os

from pkg.config import config
from pkg.model import model
from pkg.trade import trade

POSITIONS_COUNT = 10

trade_client = trade.Client(
    darqube_api_key=os.getenv("DARQUBE_API_KEY"),
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
    alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
    is_paper=os.getenv("IS_PAPER") == "true",
)

model_client = model.Client(
    model_endpoint_name=os.getenv("MODEL_ENDPOINT_NAME"),
)


def handler(
    event: any,
    context: any,
) -> dict[str, any]:
    """Set positions based on portfolio position and model predictions."""
    _ = event, context

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
        predictions_by_ticker = model_client.get_predictions()

        moves_by_ticker = {
            ticker: predictions_by_ticker[ticker][0][0] - predictions_by_ticker[ticker][4][0]
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
