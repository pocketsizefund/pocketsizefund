"""Inference endpoint for price prediction model."""
import json
import os
from datetime import datetime, timedelta

import flask

from pkg.data import data
from pkg.model import model
from pkg.trade import trade

app = flask.Flask(__name__)

<<<<<<< HEAD
trade_client = trade.Client(
    darqube_api_key=os.getenv("DARQUBE_API_KEY"),
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_secret_key=os.getenv("ALPACA_SECRET_KEY"),
    alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
    is_paper=True,
=======

scalers_file = open(os.getenv("MODEL_DIR")+"/scalers.pkl", "rb")
scalers = pickle.load(scalers_file)

model_model = model.Model(
    artifact_output_path=os.getenv("MODEL_DIR"),
    weights_and_biases_api_key="",
>>>>>>> 9e24e11 (ruff - double quotes preferred)
)

data_client = data.Client(
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_secret_key=os.getenv("ALPACA_SECRET_KEY"),
    edgar_user_agent=os.getenv("EDGAR_USER_AGENT"),
    print_logs=False,
)

price_prediction_model = model.Model()

price_prediction_model.load_model(
    file_path="price_prediction_model.ckpt",
)

<<<<<<< HEAD
=======

>>>>>>> 9e24e11 (ruff - double quotes preferred)
@app.route("/invocations", methods=["POST"])
def invocations() -> flask.Response:
    """Invocations handles prediction requests to the inference endpoint."""
    available_tickers = trade_client.get_available_tickers()

    start_date = datetime.now(
        tz="UTC",
    )
    end_date = start_date - timedelta(
        days=20,
    )

    equity_bars_raw_data = data_client.get_range_equities_bars(
        tickers=available_tickers,
        start_date=start_date,
        end_date=end_date,
    )

    predictions = price_prediction_model.predict(
        data=equity_bars_raw_data,
    )

    return flask.Response(
        response=json.dumps(predictions),
        status=200,
    )


@app.route("/ping", methods=["GET"])
def ping() -> flask.Response:
    """Ping checks the health of the model endpoint."""
    return flask.Response(
        response="",
        status=200,
        mimetype="application/json",
    )


if __name__ == "__main__":
    app.run(
<<<<<<< HEAD
        host="0.0.0.0",  # noqa: S104
=======
        host="0.0.0.0",
>>>>>>> 9e24e11 (ruff - double quotes preferred)
        port=8080,
    )
