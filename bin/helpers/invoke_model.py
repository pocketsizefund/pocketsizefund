import argparse
import datetime

import requests

from pkg.config import config
from pkg.trade import trade
from pkg.data import data
from pkg.model import model


parser = argparse.ArgumentParser(
    prog="backfill data helper script",
    description="update s3 data bucket with training data",
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

data_client = data.Client(
    alpaca_api_key=samconfig_file.get_parameter("AlpacaAPIKey"),
    alpaca_api_secret=samconfig_file.get_parameter("AlpacaAPISecret"),
    print_logs=True,
)

model_client = model.Client(
    model_endpoint_name=samconfig_file.get_parameter("ModelEndpointName"),
)

available_tickers = trade_client.get_available_tickers()

end_at = datetime.datetime.now()
start_at = end_at - datetime.timedelta(days=50)

prediction_data = data_client.get_range_equities_bars(
    tickers=available_tickers,
    start_at=start_at,
    end_at=end_at,
)

prediction_data = (
    prediction_data.sort_values(
        by="timestamp",
        ascending=False,
    )
    .groupby("ticker")
    .head(30)
    .reset_index(drop=True)
)

prediction_data["timestamp"] = prediction_data["timestamp"].astype(str)

predictions = None

if arguments.location == "remote":
    predictions = model_client.generate_predictions(
        data=prediction_data,
    )

else:
    predictions = requests.post(
        url="http://localhost:8080/invocations",
        json=prediction_data.to_dict(),
    ).json()


for ticker, ticker_predictions in predictions.items():
    closing_prices = [ticker_prediction[0] for ticker_prediction in ticker_predictions]

    print("ticker: {}".format(ticker))
    print("closing_prices: {}".format(closing_prices))
