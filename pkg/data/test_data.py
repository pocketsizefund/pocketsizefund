import unittest
import datetime

from alpaca.data import requests as alpaca_data_requests

from pkg.data import data


class MockHTTPTimeSeriesDailyAdjustedResponse:
    def __init__(
        self,
        data: dict[str, any],
    ) -> None:
        self.data = data

    def json(self) -> dict[str, any]:
        return self.data


class MockHTTPClient:
    def __init__(
        self,
        response: MockHTTPTimeSeriesDailyAdjustedResponse,
    ) -> None:
        self.response = response

    def get(
        self,
        url: str,
        params: dict[str, any],
    ) -> any:
        return self.response


class TestGetAllEquitiesBars(unittest.TestCase):
    def test_get_all_equities_bars_success(self):
        client = data.Client(
            alpha_vantage_api_key='alpha_vantage_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
        )

        client.http_client = MockHTTPClient(
            response=MockHTTPTimeSeriesDailyAdjustedResponse(
                data={
                    'Meta Data': {
                        '2. Symbol': 'TICKER',
                    },
                    'Time Series (Daily)': {
                        '2021-01-01': {
                            '1. open': '5.0',
                            '2. high': '6.0',
                            '3. low': '4.0',
                            '5. adjusted close': '5.0',
                            '6. volume': '100.0',
                        },
                    },
                },
            ),
        )

        client.alpha_vantage_delay_in_seconds = 0

        all_equities_bars = client.get_all_equities_bars(
            tickers=['TICKER'],
        )

        self.assertEqual(1, len(all_equities_bars))
        self.assertEqual('TICKER', all_equities_bars.iloc[0]['ticker'])
        self.assertEqual(5.0, all_equities_bars.iloc[0]['open_price'])
        self.assertEqual(6.0, all_equities_bars.iloc[0]['high_price'])
        self.assertEqual(4.0, all_equities_bars.iloc[0]['low_price'])
        self.assertEqual(5.0, all_equities_bars.iloc[0]['close_price'])
        self.assertEqual(100.0, all_equities_bars.iloc[0]['volume'])
        self.assertEqual('ALPHA_VANTAGE', all_equities_bars.iloc[0]['source'])


class MockAlpacaHistoricalResponse:
    def __init__(
        self,
        data: dict[str, any],
    ) -> any:
        self.data = data
        self.keys = list(self.data.keys())
        self.index = 0

    def __iter__(self) -> any:
        return self

    def __next__(self) -> any:
        if self.index < len(self.keys):
            key = self.keys[self.index]
            self.index += 1
            return key
        else:
            raise StopIteration

    def __getitem__(
        self,
        key: str,
    ) -> any:
        return self.data[key]


class MockAlpacaHistoricalClient:
    def __init__(
        self,
        response: MockAlpacaHistoricalResponse,
    ) -> None:
        self.response = response

    def get_stock_bars(
        self,
        request: alpaca_data_requests.StockBarsRequest,
    ) -> any:
        return self.response


class TestGetRangeEquitiesBars(unittest.TestCase):
    def test_get_range_equities_bars_success(self):
        client = data.Client(
            alpha_vantage_api_key='alpha_vantage_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
        )

        client.alpaca_historical_client = MockAlpacaHistoricalClient(
            response=MockAlpacaHistoricalResponse(
                data={
                    'TICKER': [
                        {
                            't': '1977-05-25T00:00:00Z',
                            'o': '5.0',
                            'h': '6.0',
                            'l': '4.0',
                            'c': '5.0',
                            'v': '100.0',
                        },
                        {
                            't': '1977-05-25T00:00:00Z',
                            'o': '6.0',
                            'h': '7.0',
                            'l': '5.0',
                            'c': '6.0',
                            'v': '200.0',
                        },
                        {
                            't': '1977-05-25T00:00:00Z',
                            'o': '7.0',
                            'h': '8.0',
                            'l': '6.0',
                            'c': '7.0',
                            'v': '300.0',
                        },
                    ]
                },
            ),
        )

        range_equities_bars = client.get_range_equities_bars(
            tickers=['TICKER'],
            start_at=datetime.datetime.strptime('1977-05-25', '%Y-%m-%d'),
            end_at=datetime.datetime.strptime('1977-05-28', '%Y-%m-%d'),
        )

        self.assertEqual(3, len(range_equities_bars))
        self.assertEqual('TICKER', range_equities_bars.iloc[0]['ticker'])
        self.assertEqual(5.0, range_equities_bars.iloc[0]['open_price'])
        self.assertEqual(6.0, range_equities_bars.iloc[0]['high_price'])
        self.assertEqual(4.0, range_equities_bars.iloc[0]['low_price'])
        self.assertEqual(5.0, range_equities_bars.iloc[0]['close_price'])
        self.assertEqual(100.0, range_equities_bars.iloc[0]['volume'])
        self.assertEqual('ALPACA', range_equities_bars.iloc[0]['source'])
