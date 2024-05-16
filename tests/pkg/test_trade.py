import datetime
import unittest

from pkg.trade import trade


class MockAlpacaClock:
    def __init__(
        self,
        is_open: bool,
    ) -> None:
        self.is_open = is_open


class MockHTTPResponse:
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
        response: MockHTTPResponse,
        exception: Exception,
    ) -> None:
        self.response = response
        self.exception = exception

    def get(
        self,
        url: str,
        params: any,
        headers: any = None,
    ) -> any:
        _ = url, params, headers

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
        equity: float,
    ) -> None:
        self.cash = cash
        self.equity = equity


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
    raise Exception("get available tickers error")


def mock_get_available_tickers_success() -> list[str]:
    return ["TICKER"]


class MockAlpacaCalendar:
    def __init__(
        self,
        date_value: datetime.date,
        open_value: datetime.datetime,
        close_value: datetime.datetime,
    ) -> None:
        self.date = date_value
        self.open = open_value
        self.close = close_value


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
        if self.exceptions is not None and self.exceptions["get_clock"] is not None:
            raise self.exceptions["get_clock"]

        return self.responses["get_clock"]

    def get_calendar(
        self,
        filters: any,
    ) -> list[any]:
        _ = filters

        return self.responses["get_calendar"]

    def get_all_positions(
        self,
    ) -> list[any]:
        return self.responses["get_all_positions"]

    def get_all_assets(
        self,
        request: any,
    ) -> any:
        if self.exceptions is not None and self.exceptions["get_all_assets"] is not None:
            raise self.exceptions["get_all_assets"]

        return self.responses["get_all_assets"]

    def get_account(self) -> any:
        if self.exceptions is not None and self.exceptions["get_account"] is not None:
            raise self.exceptions["get_account"]

        return self.responses["get_account"]

    def submit_order(
        self,
        request: any,
    ) -> any:
        if self.exceptions is not None and self.exceptions["submit_order"] is not None:
            raise self.exceptions["submit_order"]

        self.last_request = request
        return {
            "symbol": request.symbol,
            "notional": request.notional,
            "side": request.side,
        }

    def close_all_positions(
        self,
        cancel_orders: bool,
    ) -> any:
        _ = cancel_orders

        if self.exceptions is not None and self.exceptions["close_all_positions"] is not None:
            raise self.exceptions["close_all_positions"]

        return None


class MockAlpacaGetStockBarsResponse:
    def __init__(
        self,
        data: list[dict[str, any]],
    ) -> None:
        self.data = data

    def __getitem__(
        self,
        index: str,
    ) -> dict[str, any]:
        return self.data


class MockAlpacaHistoricalClient:
    def __init__(
        self,
        responses: dict[str, any],
        exceptions: dict[str, Exception],
    ) -> None:
        self.responses = responses
        self.exceptions = exceptions

    def get_stock_bars(
        self,
        request: any,
    ) -> any:
        if self.exceptions is not None and self.exceptions["get_stock_bars"] is not None:
            raise self.exceptions["get_stock_bars"]

        return self.responses["get_stock_bars"]


def mock_get_portoflio_daily_returns_error(
    week_count: int,
    end_at: datetime.datetime,
) -> list[dict[str, any]]:
    raise Exception("get portfolio returns error")


def mock_get_portfolio_returns_success(
    week_count: int,
    end_at: datetime.datetime,
) -> list[dict[str, any]]:
    return [
        -0.0082,
        -0.0002,
        0.0056,
        0.0025,
        0.0053,
    ]


def mock_get_benchmark_daily_returns_error(
    week_count: int,
    end_at: datetime.datetime,
) -> list[dict[str, any]]:
    raise Exception("get benchmark returns error")


def mock_get_benchmark_daily_returns_success(
    week_count: int,
    end_at: datetime.datetime,
) -> list[dict[str, any]]:
    return [
        0.01,
        0.0099,
        0.0098,
        0.0097,
        0.0096,
    ]


def mock_get_risk_free_rate_error() -> float:
    raise Exception("get risk free rate error")


def mock_get_risk_free_rate_success() -> float:
    return 0.03


class TestCheckSetPositionAvailability(unittest.TestCase):
    def test_check_position_set_availability_success(self) -> None:
        client = trade.Client(
            darqube_api_key="darqube_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

        monday_calendar_days = [
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 23),
                open_value=datetime.datetime(1977, 5, 23, 9, 30),
                close_value=datetime.datetime(1977, 5, 23, 16, 0),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 24),
                open_value=datetime.datetime(1977, 5, 24, 9, 30),
                close_value=datetime.datetime(1977, 5, 24, 16, 0),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 25),
                open_value=datetime.datetime(1977, 5, 25, 9, 30),
                close_value=datetime.datetime(1977, 5, 25, 16, 0),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 26),
                open_value=datetime.datetime(1977, 5, 26, 9, 30),
                close_value=datetime.datetime(1977, 5, 26, 16, 0),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 27),
                open_value=datetime.datetime(1977, 5, 27, 9, 30),
                close_value=datetime.datetime(1977, 5, 27, 16, 0),
            ),
        ]

        friday_calendar_days = [
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 27),
                open_value=datetime.datetime(1977, 5, 27, 9, 30),
                close_value=datetime.datetime(1977, 5, 27, 16, 0),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 30),
                open_value=datetime.datetime(1977, 5, 30, 9, 30),
                close_value=datetime.datetime(1977, 5, 30, 16, 0),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 31),
                open_value=datetime.datetime(1977, 5, 31, 9, 30),
                close_value=datetime.datetime(1977, 5, 31, 16, 0),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 6, 1),
                open_value=datetime.datetime(1977, 6, 1, 9, 30),
                close_value=datetime.datetime(1977, 6, 1, 16, 0),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 6, 2),
                open_value=datetime.datetime(1977, 6, 2, 9, 30),
                close_value=datetime.datetime(1977, 6, 2, 16, 0),
            ),
        ]

        tests = [
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 9, 15),
                "is_market_open": False,
                "calendar_days": monday_calendar_days,
                "all_positions": [],
                "result": False,
            },
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 26, 9, 45),
                "is_market_open": True,
                "calendar_days": [
                    MockAlpacaCalendar(
                        date_value=datetime.date(1977, 5, 26),
                        open_value=datetime.datetime(1977, 5, 26, 9, 30),
                        close_value=datetime.datetime(1977, 5, 26, 16, 0),
                    )
                ]
                + friday_calendar_days[1:],
                "all_positions": [],
                "result": True,
            },
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 27, 15, 30),
                "is_market_open": True,
                "calendar_days": friday_calendar_days,
                "all_positions": [],
                "result": False,
            },
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 9, 45),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [{}],
                "result": False,
            },
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 9, 45),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [],
                "result": True,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 16, 15),
                "is_market_open": False,
                "calendar_days": monday_calendar_days,
                "all_positions": [],
                "result": False,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 26, 15, 30),
                "is_market_open": True,
                "calendar_days": [
                    MockAlpacaCalendar(
                        date_value=datetime.date(1977, 5, 26),
                        open_value=datetime.datetime(1977, 5, 26, 9, 30),
                        close_value=datetime.datetime(1977, 5, 26, 16, 0),
                    )
                ]
                + friday_calendar_days[1:],
                "all_positions": [{}],
                "result": True,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 15, 30),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [{}],
                "result": False,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 15, 30),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [],
                "result": False,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 27, 15, 30),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [{}],
                "result": True,
            },
        ]

        for test in tests:
            client.alpaca_trading_client = MockAlpacaTradingClient(
                responses={
                    "get_clock": MockAlpacaClock(
                        is_open=test["is_market_open"],
                    ),
                    "get_calendar": test["calendar_days"],
                    "get_all_positions": test["all_positions"],
                },
                exceptions=None,
            )

            result = client.check_set_position_availability(
                action=test["action"],
                current_datetime=test["current_datetime"],
            )

            self.assertEqual(test["result"], result)


class TestPrivateGetAvailableTickers(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="darqube_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test__get_available_tickers_darqube_get_tickers_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=None,
            exception=Exception("darqube get tickers error"),
        )

        with self.assertRaises(Exception) as context:
            self.client._get_available_tickers()

        self.assertEqual(
            str(context.exception),
            "darqube get tickers error",
        )

    def test__get_available_tickers_alpaca_get_all_assets_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=MockHTTPResponse(
                data={
                    "0": {
                        "Code": "TICKER",
                    },
                },
            ),
            exception=None,
        )

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                "get_all_assets": Exception("alpaca get all assets error"),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client._get_available_tickers()

        self.assertEqual(
            str(context.exception),
            "alpaca get all assets error",
        )

    def test__get_available_tickers_success(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=MockHTTPResponse(
                data={
                    "0": {
                        "Code": "TICKER",
                    },
                },
            ),
            exception=None,
        )

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                "get_all_assets": MockAlpacaGetAllAssetsResponse(
                    data=[
                        MockAlpacaAsset(
                            symbol="TICKER",
                        ),
                    ],
                ),
            },
            exceptions=None,
        )

        tickers = self.client._get_available_tickers()

        self.assertEqual(1, len(tickers))
        self.assertEqual("TICKER", tickers[0])


class TestGetAvailableTickers(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="daruqbe_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test_get_available_tickers_error(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_error

        with self.assertRaises(Exception) as context:
            self.client.get_available_tickers()

        self.assertEqual(
            str(context.exception),
            "get available tickers error",
        )

    def test_get_available_tickers_success(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_success

        tickers = self.client.get_available_tickers()

        self.assertEqual(1, len(tickers))
        self.assertEqual("TICKER", tickers[0])


class TestSetPositions(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="darqube_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test_set_positions_get_available_tickers_error(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_error

        with self.assertRaises(Exception) as context:
            self.client.set_positions(
                tickers=["TICKER"],
            )

        self.assertEqual(
            str(context.exception),
            "get available tickers error",
        )

    def test_set_positions_get_account_error(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_success

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                "get_account": Exception("alpaca get account error"),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client.set_positions(
                tickers=["TICKER"],
            )

        self.assertEqual(
            str(context.exception),
            "alpaca get account error",
        )

    def test_set_positions_submit_order_error(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_success

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                "get_account": MockAlpacaAccount(
                    cash=100.0,
                    equity=100.0,
                ),
            },
            exceptions={
                "get_account": None,
                "submit_order": Exception("alpaca submit order error"),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client.set_positions(
                tickers=["TICKER"],
            )

        self.assertEqual(
            str(context.exception),
            "alpaca submit order error",
        )

    def test_set_positions_success(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_success

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                "get_account": MockAlpacaAccount(
                    cash=100.0,
                    equity=100.0,
                ),
            },
            exceptions={
                "get_account": None,
                "submit_order": None,
            },
        )

        self.client.set_positions(
            tickers=["TICKER"],
        )

        last_request = self.client.alpaca_trading_client.last_request
        self.assertIsNotNone(last_request)
        self.assertEqual(last_request.symbol, "TICKER")
        self.assertEqual(last_request.notional, 95.0)
        self.assertEqual(last_request.side, "buy")


class TestClearPositions(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="daruqbe_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test_clear_positions_close_all_positions_error(self) -> None:
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                "close_all_positions": Exception("alpaca close all positions error"),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client.clear_positions()

        self.assertEqual(
            str(context.exception),
            "alpaca close all positions error",
        )

    def test_clear_positions_success(self) -> None:
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions=None,
        )

        self.client.clear_positions()

        self.assertTrue(True)


class TestPrivateGetPortfolioDailyReturns(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="darqube_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test__get_portfolio_daily_returns_http_client_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=None,
            exception=Exception("alpaca get portfolio returns error"),
        )

        with self.assertRaises(Exception) as context:
            self.client._get_portoflio_daily_returns(
                week_count=1,
                end_at=datetime.datetime.now(),
            )

        self.assertEqual(
            str(context.exception),
            "alpaca get portfolio returns error",
        )

    def test__get_portfolio_daily_returns_insufficient_data_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=MockHTTPResponse(
                data={"timestamp": []},
            ),
            exception=None,
        )

        with self.assertRaises(Exception) as context:
            self.client._get_portoflio_daily_returns(
                week_count=1,
                end_at=datetime.datetime.now(),
            )

        self.assertEqual(
            str(context.exception),
            "insufficient portfolio data",
        )

    def test__get_portfolio_daily_returns_success(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=MockHTTPResponse(
                data={
                    "timestamp": [
                        1708995600,
                        1709082000,
                        1709168400,
                        1709254800,
                        1709341200,
                    ],
                    "profit_loss_pct": [
                        -0.0082,
                        -0.0002,
                        0.0056,
                        0.0025,
                        0.0053,
                    ],
                },
            ),
            exception=None,
        )

        returns = self.client._get_portoflio_daily_returns(
            week_count=1,
            end_at=datetime.datetime.now(),
        )

        self.assertEqual(5, len(returns))
        self.assertEqual(-0.0082, returns[0])


class TestPrivateGetBenchmarkDailyReturns(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="darqube_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test__get_benchmark_daily_returns_alpaca_client_error(self) -> None:
        self.client.alpaca_historical_client = MockAlpacaHistoricalClient(
            responses=None,
            exceptions={
                "get_stock_bars": Exception("alpaca get benchmark returns error"),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client._get_benchmark_daily_returns(
                week_count=1,
                end_at=datetime.datetime.now(),
            )

        self.assertEqual(
            str(context.exception),
            "alpaca get benchmark returns error",
        )

    def test__get_benchmark_daily_returns_insufficient_data_error(self) -> None:
        self.client.alpaca_historical_client = MockAlpacaHistoricalClient(
            responses={
                "get_stock_bars": {
                    "SPY": [],
                }
            },
            exceptions=None,
        )

        with self.assertRaises(Exception) as context:
            self.client._get_benchmark_daily_returns(
                week_count=1,
                end_at=datetime.datetime.now(),
            )

        self.assertEqual(
            str(context.exception),
            "insufficient benchmark data",
        )

    def test__get_benchmark_daily_returns_success(self) -> None:
        self.client.alpaca_historical_client = MockAlpacaHistoricalClient(
            responses={
                "get_stock_bars": MockAlpacaGetStockBarsResponse(
                    data=[
                        {
                            "c": 100.0,
                        },
                        {
                            "c": 101.0,
                        },
                        {
                            "c": 102.0,
                        },
                        {
                            "c": 103.0,
                        },
                        {
                            "c": 104.0,
                        },
                        {
                            "c": 105.0,
                        },
                    ],
                ),
            },
            exceptions=None,
        )

        returns = self.client._get_benchmark_daily_returns(
            week_count=1,
            end_at=datetime.datetime.now(),
        )

        self.assertEqual(5, len(returns))
        self.assertEqual([0.01, 0.0099, 0.0098, 0.0097, 0.0096], returns)


class TestPrivateCumulativeReturns(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="darqube_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test__cumulative_returns_success(self) -> None:
        returns = [0.01, 0.0099, 0.0098, 0.0097, 0.0096]

        cumulative_returns = self.client._cumulative_returns(
            returns=returns,
        )

        self.assertEqual(0.05, cumulative_returns)


class TestPrivateGetRiskFreeRate(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="darqube_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test__get_risk_free_rate_http_client_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=None,
            exception=Exception("get risk free rate error"),
        )

        with self.assertRaises(Exception) as context:
            self.client._get_risk_free_rate()

        self.assertEqual(
            str(context.exception),
            "get risk free rate error",
        )

    def test__get_risk_free_rate_success(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=MockHTTPResponse(
                data={
                    "data": [
                        {
                            "date": "2021-01-01",
                            "value": "1.0",
                        },
                        {
                            "date": "2021-02-01",
                            "value": "2.0",
                        },
                        {
                            "date": "2021-03-01",
                            "value": "3.0",
                        },
                    ],
                },
            ),
            exception=None,
        )

        risk_free_rate = self.client._get_risk_free_rate()

        self.assertEqual(0.03, risk_free_rate)


class TestGetPerformanceMetrics(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key="darqube_api_key",
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            alpha_vantage_api_key="alpha_vantage_api_key",
        )

    def tearDown(self) -> None:
        pass

    def test_get_performance_metrics_get_account_error(self) -> None:
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                "get_account": Exception("alpaca get account error"),
            },
        )

        with self.assertRaises(Exception) as context:
            self.client.get_performance_metrics(
                week_count=1,
                end_at=datetime.datetime.now(),
            )

        self.assertEqual(
            str(context.exception),
            "alpaca get account error",
        )

    def test_get_performance_metrics_get_portfolio_returns_error(self) -> None:
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                "get_account": MockAlpacaAccount(
                    cash=100.0,
                    equity=100.0,
                ),
            },
            exceptions=None,
        )

        self.client._get_portoflio_daily_returns = mock_get_portoflio_daily_returns_error

        with self.assertRaises(Exception) as context:
            self.client.get_performance_metrics(
                week_count=1,
                end_at=datetime.datetime.now(),
            )

        self.assertEqual(
            str(context.exception),
            "get portfolio returns error",
        )

    def test_get_performance_metrics_get_benchmark_daily_returns_error(self) -> None:
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                "get_account": MockAlpacaAccount(
                    cash=100.0,
                    equity=100.0,
                ),
            },
            exceptions=None,
        )

        self.client._get_portoflio_daily_returns = mock_get_portfolio_returns_success
        self.client._get_benchmark_daily_returns = mock_get_benchmark_daily_returns_error

        with self.assertRaises(Exception) as context:
            self.client.get_performance_metrics(
                week_count=1,
                end_at=datetime.datetime.now(),
            )

        self.assertEqual(
            str(context.exception),
            "get benchmark returns error",
        )

    def test_get_performance_metrics_get_risk_free_rate_error(self) -> None:
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                "get_account": MockAlpacaAccount(
                    cash=100.0,
                    equity=100.0,
                ),
            },
            exceptions=None,
        )

        self.client._get_portoflio_daily_returns = mock_get_portfolio_returns_success
        self.client._get_benchmark_daily_returns = mock_get_benchmark_daily_returns_success
        self.client._get_risk_free_rate = mock_get_risk_free_rate_error

        with self.assertRaises(Exception) as context:
            self.client.get_performance_metrics(
                week_count=1,
                end_at=datetime.datetime.now(),
            )

        self.assertEqual(
            str(context.exception),
            "get risk free rate error",
        )

    def test_get_performance_metrics_success(self) -> None:
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses={
                "get_account": MockAlpacaAccount(
                    cash=100.0,
                    equity=100.0,
                ),
            },
            exceptions=None,
        )

        self.client._get_portoflio_daily_returns = mock_get_portfolio_returns_success
        self.client._get_benchmark_daily_returns = mock_get_benchmark_daily_returns_success
        self.client._get_risk_free_rate = mock_get_risk_free_rate_success

        metrics = self.client.get_performance_metrics(
            week_count=1,
            end_at=datetime.datetime.now(),
        )

        self.assertEqual(100.0, metrics["current_portfolio_value"])
        self.assertEqual(0.0049, metrics["cumulative_portfolio_returns"])
        self.assertEqual(0.05, metrics["cumulative_benchmark_returns"])
        self.assertEqual(0.03, metrics["risk_free_rate"])
        self.assertEqual(0.03, metrics["risk_free_rate"])
