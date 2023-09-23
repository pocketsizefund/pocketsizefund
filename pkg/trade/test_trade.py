import unittest

from pkg.trade import trade

class TestTrade(unittest.TestCase):

    def setUp(self):
        self.client = trade.Client(
            darqube_api_key='darqube_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
        )

    def tearDown(self):
        pass


    def test_get_market_status_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response=MockAlpacaGetClockResponse(
                is_open=True,
            ),
        )

        market_status = self.client.get_market_status()

        self.assertEqual(True, market_status['is_market_open'])


    def test_get_available_tickers_success(self):
        self.client.http_client = MockHTTPClient(
            response=MockDarqubeGetTickersResponse(
                data={
                    '0': {
                        'Code': 'TICKER',
                    },
                },
            ),
        )

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response=MockAlpacaGetAllAssetsResponse(
                data=[
                    MockAlpacaAsset(
                        symbol='TICKER',
                    ),
                ],
            ),
        )

        tickers = self.client.get_available_tickers()

        self.assertEqual(1, len(tickers))
        self.assertEqual('TICKER', tickers[0])


    def test_get_available_cash_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response=MockAlpacaAccount(
                cash=100.0,
            ),
        )

        available_cash = self.client.get_available_cash()

        self.assertEqual(100.0, available_cash)


    def test_get_current_prices_success(self):
        self.client.alpaca_data_client = MockAlpacaDataClient(
            response=MockAlpacaGetStockLatestTradesResponse(
                data={
                    'TICKER': MockAlpacaTrade(
                        price=5.0,
                    ),
                },
            ),
        )

        current_prices = self.client.get_current_prices(
            tickers=['TICKER'],
        )

        self.assertEqual(1, len(current_prices))
        self.assertEqual(5.0, current_prices['TICKER']['price'])


    def test_set_position_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response=None,
        )

        self.client.set_positions(
            positions=[
                {
                    'ticker': 'TICKER',
                    'quantity': 10.0,
                    'side': trade.SIDE_BUY,
                },
            ],
        )


    def test_clear_positions_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response=None,
        )

        self.client.clear_positions()



class MockAlpacaGetClockResponse:
    def __init__(
        self,
        is_open: bool,
    ) -> None:
        self.is_open = is_open


class MockDarqubeGetTickersResponse:
    def __init__(
        self,
        data: dict[str, any],
    ) -> None:
        self.data = data

    def json(self) -> any:
        return self.data


class MockHTTPClient:
    def __init__(
        self,
        response: MockDarqubeGetTickersResponse,
    ) -> None:
        self.response = response

    def get(
        self,
        url: str,
        params: any,
    ) -> any:
        return self.response


class MockAlpacaAsset:
    def __init__(
        self,
        symbol: str,
    ) -> None:
        self.symbol = symbol
        self.tradable = True
        self.fractionable = True
        self.shortable = True


class MockAlpacaAccount:
    def __init__(
        self,
        cash: float,
    ) -> None:
        self.cash = cash


class MockAlpacaGetAllAssetsResponse:
    def __init__(
        self,
        data: list[MockAlpacaAsset],
    ) -> any:
        self.data = data
        self.index = 0

    def __iter__(self) -> any:
        return self

    def __next__(self) -> any:
        if self.index < len(self.data):
            value = self.data[self.index]
            self.index += 1
            return value
        else:
            raise StopIteration


class MockAlpacaTradingClient:
    def __init__(
        self,
        response: any,
    ) -> None:
        self.response = response

    def get_all_assets(
        self,
        request: any,
    ) -> any:
        return self.response

    def get_clock(self) -> any:
        return self.response

    def get_account(self) -> any:
        return self.response

    def submit_order(
        self,
        request: any,
    ) -> any:
        return self.response

    def close_all_positions(
        self,
        cancel_orders: bool,
    ) -> any:
        return self.response
    

class MockAlpacaTrade:
    def __init__(
        self,
        price: float,
    ) -> None:
        self.price = price


class MockAlpacaGetStockLatestTradesResponse:
    def __init__(
        self,
        data: dict[str, MockAlpacaTrade],
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


class MockAlpacaDataClient:
    def __init__(
        self,
        response: any,
    ) -> None:
        self.response = response

    def get_stock_latest_trade(
        self,
        request: any,
    ) -> any:
        return self.response
