"""Inference endpoint for price prediction model."""

import datetime
import json
import os

import flask

from pkg.config import config
from pkg.data import data
from pkg.model import model
from pkg.trade import trade

app = flask.Flask(__name__)

trade_client = trade.Client(
    darqube_api_key=os.getenv("DARQUBE_API_KEY"),
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
    alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
    is_paper=True,
)

data_client = data.Client(
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
    edgar_user_agent=os.getenv("EDGAR_USER_AGENT"),
    debug=False,
)

model_model = model.Model()

model_model.load_model(
    file_path=os.getenv("MODEL_FILE_NAME"),
)


@app.route("/predictions", methods=["GET"])
def invocations() -> flask.Response:
    """Invocations handles prediction requests to the inference endpoint."""
    available_tickers = trade_client.get_available_tickers()

    end_at = datetime.datetime.now(tz=config.TIMEZONE)
    start_at = end_at - datetime.timedelta(days=20)

    equity_bars_raw_data = data_client.get_range_equities_bars(
        tickers=available_tickers,
        start_at=start_at,
        end_at=end_at,
    )

    equity_bars_raw_data_grouped_by_ticker = equity_bars_raw_data.groupby("ticker")

    predictions = {}
    for ticker, ticker_bars_raw_data in equity_bars_raw_data_grouped_by_ticker:
        ticker_predictions = model_model.get_predictions(
            data=ticker_bars_raw_data,
        )

        predictions[ticker] = ticker_predictions[0]

    return flask.Response(
        response=json.dumps(predictions),
        status=200,
    )


if __name__ == "__main__":
    app.run(
        host="0.0.0.0",  # noqa: S104
        port=8080,
    )
