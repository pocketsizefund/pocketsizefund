import datetime

from sagemaker import tensorflow

from pkg.trade import trade
from pkg.data import data


class Client:
    def __init__(
        self,
        model_endpoint_name: str,
        darqube_api_key: str,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        alpha_vantage_api_key: str,
    ) -> None:
        self.predictor = tensorflow.TensorFlowPredictor(
            endpoint_name=model_endpoint_name,
        )

        self.trade_client = trade.Client(
            darqube_api_key=darqube_api_key,
            alpaca_api_key=alpaca_api_key,
            alpaca_api_secret=alpaca_api_secret,
        )

        self.data_client = data.Client(
            alpha_vantage_api_key=alpha_vantage_api_key,
            alpaca_api_key=alpaca_api_key,
            alpaca_api_secret=alpaca_api_secret,
        )

    def generate_predictions(self) -> any:
        available_tickers = self.trade_client.get_available_tickers()

        end_at = datetime.datetime.now()
        start_at = end_at - datetime.timedelta(days=1)

        equity_bars = self.data_client.get_range_equities_bars(
            tickers=available_tickers,
            start_at=start_at,
            end_at=end_at,
        )

        equity_bars['timestamp'] = equity_bars['timestamp'].astype(str)

        predictions = self.predictor.predict(
            data=equity_bars.to_dict(orient='records'),
        )

        return predictions
