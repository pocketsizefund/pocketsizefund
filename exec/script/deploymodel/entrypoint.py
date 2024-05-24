"""Inference endpoint for price prediction model."""

import json
import os

import flask

from pkg.data import data
from pkg.model import model
from pkg.trade import trade

app = flask.Flask(__name__)

model_model = model.Model(
    artifact_output_path=os.getenv("MODEL_DIR"),
    weights_and_biases_api_key="",
)

trade_client = trade.Client(
    darqube_api_key=os.getenv("DARQUBE_API_KEY"),
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_secret_key=os.getenv("ALPACA_SECRET_KEY"),
    alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
    is_paper=True,
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


@app.route("/invocations", methods=["POST"])
def invocations() -> flask.Response:
    """Invocations handles prediction requests to the inference endpoint."""
    predictions = price_prediction_model.get_predictions()

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
        host="0.0.0.0",  # noqa: S104
        port=8080,
    )
