import unittest
import datetime

import pandas

from pkg.model import model


class MockPredictor:
    def __init__(self) -> None:
        pass

    def predict(
        self,
        data: any,
    ) -> any:
        return {
            '65': [
                [
                    10.0,
                ],
            ],
        }


class MockTradeClient:
    def __init__(self) -> None:
        pass

    def get_available_tickers(self) -> list[str]:
        return ['AAPL']


class MockDataClient:
    def __init__(self) -> None:
        pass

    def get_range_equities_bars(
        self,
        tickers: list[str],
        start_at: datetime,
        end_at: datetime
    ) -> pandas.DataFrame:
        return pandas.DataFrame(
            data={
                'timestamp': pandas.Timestamp('2021-01-01'),
            },
            index=[0],
        )


class TestGeneratePredictions(unittest.TestCase):
    def test_generate_predictions_success(self):
        client = model.Client(
            model_endpoint_name='model_endpoint_name',
            darqube_api_key='darqube_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
            alpha_vantage_api_key='alpha_vantage_api_key',
        )

        client.predictor = MockPredictor()
        client.trade_client = MockTradeClient()
        client.data_client = MockDataClient()

        predictions = client.generate_predictions()

        self.assertEqual(10.0, predictions['65'][0][0])
