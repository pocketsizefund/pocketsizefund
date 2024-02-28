import unittest

from pkg.trade import trade


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
        exception: Exception,
    ) -> None:
        self.response = response
        self.exception = exception

    def get(
        self,
        url: str,
        params: any,
    ) -> any:
        _ = url, params

        if self.exception is not None:
            raise self.exception

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


def mock_get_available_tickers_error() -> list[str]:
    raise Exception('get available tickers error')


def mock_get_available_tickers_success() -> list[str]:
    return ['TICKER']


class MockAlpacaTradingClient:
    def __init__(
        self,
        responses: dict[str, any],
        exceptions: dict[str, Exception],
    ) -> None:
        self.responses = responses
        self.exceptions = exceptions
        self.last_request = None

    def get_clock(self) -> any:
        if self.exceptions is not None and self.exceptions['get_clock'] is not None:
            raise self.exceptions['get_clock']

        return self.responses['get_clock']

    def get_all_assets(
        self,
        request: any,
    ) -> any:
        if self.exceptions is not None and self.exceptions['get_all_assets'] is not None:
            raise self.exceptions['get_all_assets']

        return self.responses['get_all_assets']

    def get_account(self) -> any:
        if self.exceptions is not None and self.exceptions['get_account'] is not None:
            raise self.exceptions['get_account']

        return self.responses['get_account']

    def submit_order(
        self,
        request: any,
    ) -> any:
        if self.exceptions is not None and self.exceptions['submit_order'] is not None:
            raise self.exceptions['submit_order']

        self.last_request = request
        return {
            'symbol': request.symbol,
            'notional': request.notional,
            'side': request.side,
        }

    def close_all_positions(
        self,
        cancel_orders: bool,
    ) -> any:
        _ = cancel_orders

        if self.exceptions is not None and self.exceptions['close_all_positions'] is not None:
            raise self.exceptions['close_all_positions']

        return None


class TestIsMarketOpen(unittest.TestCase):
    def setUp(self):
        self.client = trade.Client(
            darqube_api_key='darqube_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
        )

    def tearDown(self):
        pass

    def test_is_market_open_alpaca_get_clock_error(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                'get_clock': Exception('alpaca get clock error'),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client.is_market_open()

        self.assertEqual(
            str(context.exception),
            'alpaca get clock error',
        )

    def test_is_market_open_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                'get_clock': MockAlpacaGetClockResponse(
                    is_open=True,
                ),
            },
            exceptions=None,
        )

        is_market_open = self.client.is_market_open()

        self.assertEqual(True, is_market_open)


class TestPrivateGetAvailableTickers(unittest.TestCase):
    def setUp(self):
        self.client = trade.Client(
            darqube_api_key='darqube_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
        )

    def tearDown(self):
        pass

    def test__get_available_tickers_darqube_get_tickers_error(self):
        self.client.http_client = MockHTTPClient(
            response=None,
            exception=Exception('darqube get tickers error'),
        )

        with self.assertRaises(Exception) as context:
            self.client._get_available_tickers()

        self.assertEqual(
            str(context.exception),
            'darqube get tickers error',
        )

    def test__get_available_tickers_alpaca_get_all_assets_error(self):
        self.client.http_client = MockHTTPClient(
            response=MockDarqubeGetTickersResponse(
                data={
                    '0': {
                        'Code': 'TICKER',
                    },
                },
            ),
            exception=None,
        )

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                'get_all_assets': Exception('alpaca get all assets error'),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client._get_available_tickers()

        self.assertEqual(
            str(context.exception),
            'alpaca get all assets error',
        )

    def test__get_available_tickers_success(self):
        self.client.http_client = MockHTTPClient(
            response=MockDarqubeGetTickersResponse(
                data={
                    '0': {
                        'Code': 'TICKER',
                    },
                },
            ),
            exception=None,
        )

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                'get_all_assets': MockAlpacaGetAllAssetsResponse(
                    data=[
                        MockAlpacaAsset(
                            symbol='TICKER',
                        ),
                    ],
                ),
            },
            exceptions=None,
        )

        tickers = self.client._get_available_tickers()

        self.assertEqual(1, len(tickers))
        self.assertEqual('TICKER', tickers[0])


class TestGetAvailableTickers(unittest.TestCase):
    def setUp(self):
        self.client = trade.Client(
            darqube_api_key='daruqbe_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
        )

    def tearDown(self):
        pass

    def test_get_available_tickers_error(self):
        self.client._get_available_tickers = mock_get_available_tickers_error

        with self.assertRaises(Exception) as context:
            self.client.get_available_tickers()

        self.assertEqual(
            str(context.exception),
            'get available tickers error',
        )

    def test_get_available_tickers_success(self):
        self.client._get_available_tickers = mock_get_available_tickers_success

        tickers = self.client.get_available_tickers()

        self.assertEqual(1, len(tickers))
        self.assertEqual('TICKER', tickers[0])


class TestSetPositions(unittest.TestCase):
    def setUp(self):
        self.client = trade.Client(
            darqube_api_key='darqube_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
        )

    def tearDown(self):
        pass

    def test_set_positions_get_available_tickers_error(self):
        self.client._get_available_tickers = mock_get_available_tickers_error

        with self.assertRaises(Exception) as context:
            self.client.set_positions(
                tickers=['TICKER'],
            )

        self.assertEqual(
            str(context.exception),
            'get available tickers error',
        )

    def test_set_positions_get_account_error(self):
        self.client._get_available_tickers = mock_get_available_tickers_success

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                'get_account': Exception('alpaca get account error'),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client.set_positions(
                tickers=['TICKER'],
            )

        self.assertEqual(
            str(context.exception),
            'alpaca get account error',
        )

    def test_set_positions_submit_order_error(self):
        self.client._get_available_tickers = mock_get_available_tickers_success

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                'get_account': MockAlpacaAccount(
                    cash=100.0,
                ),
            },
            exceptions={
                'get_account': None,
                'submit_order': Exception('alpaca submit order error'),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client.set_positions(
                tickers=['TICKER'],
            )

        self.assertEqual(
            str(context.exception),
            'alpaca submit order error',
        )

    def test_set_positions_success(self):
        self.client._get_available_tickers = mock_get_available_tickers_success

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                'get_account': MockAlpacaAccount(
                    cash=100.0,
                ),
            },
            exceptions={
                'get_account': None,
                'submit_order': None,
            },
        )

        self.client.set_positions(
            tickers=['TICKER'],
        )

        last_request = self.client.alpaca_trading_client.last_request
        self.assertIsNotNone(last_request)
        self.assertEqual(last_request.symbol, 'TICKER')
        self.assertEqual(last_request.notional, 100.0)
        self.assertEqual(last_request.side, 'buy')


class TestClearPositions(unittest.TestCase):
    def setUp(self):
        self.client = trade.Client(
            darqube_api_key='daruqbe_api_key',
            alpaca_api_key='alpaca_api_key',
            alpaca_api_secret='alpaca_api_secret',
        )

    def tearDown(self):
        pass

    def test_clear_positions_close_all_positions_error(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                'close_all_positions': Exception('alpaca close all positions error'),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client.clear_positions()

        self.assertEqual(
            str(context.exception),
            'alpaca close all positions error',
        )

    def test_clear_positions_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions=None,
        )

        self.client.clear_positions()

        self.assertTrue(True)
