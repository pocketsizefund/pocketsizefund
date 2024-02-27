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


class MockAlpacaPosition:
    def __init__(
        self,
        symbol: str,
        qty: float,
    ) -> None:
        self.symbol = symbol
        self.qty = qty


class MockAlpacaClosePositionResponse:
    def __init__(
        self,
        symbol: str,
    ) -> None:
        self.symbol = symbol


class MockAlpacaTradingClient:
    def __init__(
        self,
        response: any,
        exception: Exception,
    ) -> None:
        self.response = response
        self.exception = exception
        self.last_request = None

    def get_all_assets(
        self,
        request: any,
    ) -> any:
        return self.response

    def get_clock(self) -> any:
        if self.exception is not None:
            raise self.exception

        return self.response

    def get_account(self) -> any:
        if self.exception is not None:
            raise self.exception

        return self.response

    def submit_order(
        self,
        request: any,
    ) -> any:
        if self.exception is not None:
            raise self.exception

        self.last_request = request
        return {
            'symbol': request.symbol,
            'qty': request.qty,
            'side': request.side,
        }

    def get_all_positions(self) -> list[MockAlpacaPosition]:
        return self.response['get_all_positions']

    def close_all_positions(
        self,
        cancel_orders: bool,
    ) -> list[MockAlpacaClosePositionResponse]:
        return self.response['close_all_positions']


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
        exception: Exception = None,
    ) -> None:
        self.response = response
        self.exception = exception

    def get_stock_latest_trade(
        self,
        request: any,
    ) -> any:
        if self.exception is not None:
            raise self.exception

        return self.response


class TestTrade(unittest.TestCase):
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
            response=None,
            exception=Exception('alpaca get clock error'),
        )

        with self.assertRaises(Exception) as context:
            self.client.is_market_open()

        self.assertEqual(
            str(context.exception),
            'alpaca get clock error',
        )

    def test_is_market_open_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response=MockAlpacaGetClockResponse(
                is_open=True,
            ),
            exception=None,
        )

        is_market_open = self.client.is_market_open()

        self.assertEqual(True, is_market_open)

    def test_get_available_tickers_darqube_get_tickers_error(self):
        self.client.http_client = MockHTTPClient(
            response=None,
            exception=Exception('darqube get tickers error'),
        )

        with self.assertRaises(Exception) as context:
            self.client.get_available_tickers()

        self.assertEqual(
            str(context.exception),
            'darqube get tickers error',
        )

    def test_get_available_tickers_success(self):
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
            response=MockAlpacaGetAllAssetsResponse(
                data=[
                    MockAlpacaAsset(
                        symbol='TICKER',
                    ),
                ],
            ),
            exception=None,
        )

        tickers = self.client.get_available_tickers()

        self.assertEqual(1, len(tickers))
        self.assertEqual('TICKER', tickers[0])

    def test_get_available_cash_alpaca_get_account_error(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response=None,
            exception=Exception('alpaca get account error'),
        )

        with self.assertRaises(Exception) as context:
            self.client.get_available_cash()

        self.assertEqual(
            str(context.exception),
            'alpaca get account error',
        )

    def test_get_available_cash_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response=MockAlpacaAccount(
                cash=100.0,
            ),
            exception=None,
        )

        available_cash = self.client.get_available_cash()

        self.assertEqual(100.0, available_cash)

    def test_get_current_prices_alpaca_get_stock_latest_trade_error(self):
        self.client.alpaca_data_client = MockAlpacaDataClient(
            response=None,
            exception=Exception('alpaca get stock latest trade error'),
        )

        with self.assertRaises(Exception) as context:
            self.client.get_current_prices(
                tickers=['TICKER'],
            )

        self.assertEqual(
            str(context.exception),
            'alpaca get stock latest trade error',
        )

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

    def test_set_positions_negative_quantity_error(self):
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
            response=MockAlpacaGetAllAssetsResponse(
                data=[
                    MockAlpacaAsset(
                        symbol='TICKER',
                    ),
                ],
            ),
            exception=None,
        )

        negative_position = [
            {
                'ticker': 'TICKER',
                'quantity': -10.0,
                'side': trade.SIDE_BUY,
            },
        ]

        with self.assertRaises(Exception) as context:
            self.client.set_positions(positions=negative_position)

        self.assertEqual(
            str(context.exception),
            'quantity must be a positive number',
        )

    def test_set_positions_invalid_ticker_error(self):
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
            response=MockAlpacaGetAllAssetsResponse(
                data=[
                    MockAlpacaAsset(
                        symbol='TICKER',
                    ),
                ],
            ),
            exception=None,
        )

        non_ticker_position = [
            {
                'ticker': 'INVALID',
                'quantity': 10,
                'side': trade.SIDE_BUY
            }
        ]

        with self.assertRaises(Exception) as context:
            self.client.set_positions(positions=non_ticker_position)

        self.assertEqual(
            str(context.exception),
            'invalid ticker "{}"'.format(non_ticker_position[0]['ticker']),
        )

    def test_set_positions_success(self):
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
            response=MockAlpacaGetAllAssetsResponse(
                data=[
                    MockAlpacaAsset(
                        symbol='TICKER',
                    ),
                ],
            ),
            exception=None,
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

        last_request = self.client.alpaca_trading_client.last_request
        self.assertIsNotNone(last_request)
        self.assertEqual(last_request.symbol, 'TICKER')
        self.assertEqual(last_request.qty, 10)
        self.assertEqual(last_request.side, 'buy')

    def test_close_all_positions_exception(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response={
                'close_all_positions': list[
                    MockAlpacaClosePositionResponse(symbol='FIRST_TICKER'),
                    MockAlpacaClosePositionResponse(symbol='SECOND_TICKER'),
                ],
                'get_all_positions': list[MockAlpacaPosition(
                    symbol='SECOND_TICKER',
                    qty=10.0,
                )],
            },
            exception=None,
        )

        with self.assertRaises(Exception) as context:
            self.client.clear_positions()

    def test_close_all_positions_success(self):
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            response={
                'close_all_positions': list[
                    MockAlpacaClosePositionResponse(symbol='FIRST_TICKER'),
                    MockAlpacaClosePositionResponse(symbol='SECOND_TICKER'),
                ],
                'get_all_positions': [],
            },
            exception=None,
        )

        self.client.clear_positions()

        self.assertTrue(True)
