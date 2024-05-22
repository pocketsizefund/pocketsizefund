"""Invoke trained model."""

import argparse

import requests

from pkg.config import config
from pkg.model import model
from pkg.trade import trade

parser = argparse.ArgumentParser(
    prog="invoke model helper script",
)

parser.add_argument(
    "--samconfig-file-path",
    type=str,
    required=True,
    dest="samconfig_file_path",
)

parser.add_argument(
    "--location",
    type=str,
    required=True,
    dest="location",
)

arguments = parser.parse_args()

samconfig_file = config.SAMConfig(
    file_path=arguments.samconfig_file_path,
    environment=config.ENVIRONMENT_DEVELOPMENT,
)

trade_client = trade.Client(
    darqube_api_key=samconfig_file.get_parameter("DarqubeAPIKey"),
    alpaca_api_key=samconfig_file.get_parameter("AlpacaAPIKey"),
    alpaca_api_secret=samconfig_file.get_parameter("AlpacaAPISecret"),
    is_paper=True,
)

model_client = model.Client(
    model_endpoint_name=samconfig_file.get_parameter("ModelEndpointName"),
)

predictions = None

if arguments.location == "remote":
    predictions = model_client.get_predictions()

else:
    predictions = requests.post(
        url="http://localhost:8080/invocations",
        timeout=60,
    ).json()


for ticker, ticker_predictions in predictions.items():
    closing_prices = [ticker_prediction[0] for ticker_prediction in ticker_predictions]

    print(f"ticker: {ticker}")  # noqa: T201
    print(f"closing_prices: {closing_prices}")  # noqa: T201
