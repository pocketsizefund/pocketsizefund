import unittest
import datetime

from alpaca.data import requests as alpaca_data_requests

from pkg.data import data


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
        exception: Exception,
    ) -> None:
        self.response = response
        self.exception = exception

    def get_stock_bars(
        self,
        request: alpaca_data_requests.StockBarsRequest,
    ) -> any:
        if self.exception is not None:
            raise self.exception

        return self.response


class TestGetRangeEquitiesBars(unittest.TestCase):
    def test_get_range_equities_bars_alpaca_get_stock_bars_error(self):
        client = data.Client(
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
        )

        client.alpaca_historical_client = MockAlpacaHistoricalClient(
            response=None,
            exception=Exception("get stock bars error"),
        )

        with self.assertRaises(Exception) as context:
            _ = client.get_range_equities_bars(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
            )

        self.assertEqual("get stock bars error", str(context.exception))

    def test_get_range_equities_bars_success(self):
        client = data.Client(
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
        )

        client.alpaca_historical_client = MockAlpacaHistoricalClient(
            response=MockAlpacaHistoricalResponse(
                data={
                    "TICKER": [
                        {
                            "t": "1977-05-25T00:00:00Z",
                            "o": "5.0",
                            "h": "6.0",
                            "l": "4.0",
                            "c": "5.0",
                            "v": "100.0",
                        },
                        {
                            "t": "1977-05-25T00:00:00Z",
                            "o": "6.0",
                            "h": "7.0",
                            "l": "5.0",
                            "c": "6.0",
                            "v": "200.0",
                        },
                        {
                            "t": "1977-05-25T00:00:00Z",
                            "o": "7.0",
                            "h": "8.0",
                            "l": "6.0",
                            "c": "7.0",
                            "v": "300.0",
                        },
                    ]
                },
            ),
            exception=None,
        )

        range_equities_bars = client.get_range_equities_bars(
            tickers=["TICKER"],
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
        )

        self.assertEqual(3, len(range_equities_bars))
        self.assertEqual("TICKER", range_equities_bars.iloc[0]["ticker"])
        self.assertEqual(5.0, range_equities_bars.iloc[0]["open_price"])
        self.assertEqual(6.0, range_equities_bars.iloc[0]["high_price"])
        self.assertEqual(4.0, range_equities_bars.iloc[0]["low_price"])
        self.assertEqual(5.0, range_equities_bars.iloc[0]["close_price"])
        self.assertEqual(100.0, range_equities_bars.iloc[0]["volume"])
        self.assertEqual("ALPACA", range_equities_bars.iloc[0]["source"])
