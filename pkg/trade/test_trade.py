import datetime
import unittest

import pytest

from pkg.config import config
from pkg.trade import trade

DARQUBE_API_KEY = "darqube_api_key"  # noqa: S106
ALPACA_API_KEY = "alpaca_api_key"  # noqa: S106
ALPACA_API_SECRET = "alpaca_api_secret"  # noqa: S106
ALPHA_VANTAGE_API_KEY = "alpha_vantage_api_key"  # noqa: S106


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

        raise StopIteration


def mock_get_available_tickers_error() -> list[str]:
    msg = "get available tickers error"
    raise ValueError(msg)


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
        request: any, # noqa: ARG002
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
        request: any, # noqa: ARG002
    ) -> any:
        if self.exceptions is not None and self.exceptions["get_stock_bars"] is not None:
            raise self.exceptions["get_stock_bars"]

        return self.responses["get_stock_bars"]


def mock_get_portoflio_daily_returns_error(
    week_count: int, # noqa: ARG001
    end_at: datetime.datetime, # noqa: ARG001
) -> list[dict[str, any]]:
    msg = "get portfolio returns error"
    raise ValueError(msg)


def mock_get_portfolio_returns_success(
    week_count: int, # noqa: ARG001
    end_at: datetime.datetime, # noqa: ARG001
) -> list[dict[str, any]]:
    return [
        -0.0082,
        -0.0002,
        0.0056,
        0.0025,
        0.0053,
    ]


def mock_get_benchmark_daily_returns_error(
    week_count: int, # noqa: ARG001
    end_at: datetime.datetime, # noqa: ARG001
) -> list[dict[str, any]]:
    msg = "get benchmark returns error"
    raise ValueError(msg)


def mock_get_benchmark_daily_returns_success(
    week_count: int, # noqa: ARG001
    end_at: datetime.datetime, # noqa: ARG001
) -> list[dict[str, any]]:
    return [
        0.01,
        0.0099,
        0.0098,
        0.0097,
        0.0096,
    ]


def mock_get_risk_free_rate_error() -> float:
    msg = "get risk free rate error"
    raise ValueError(msg)


def mock_get_risk_free_rate_success() -> float:
    return 0.03


class TestCheckSetPositionAvailability(unittest.TestCase):
    def test_check_position_set_availability_success(self) -> None:
        client = trade.Client(
            darqube_api_key=DARQUBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
        )

        monday_calendar_days = [
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 23, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 5, 23, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 5, 23, 16, 0, tzinfo=config.TIMEZONE),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 24, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 5, 24, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 5, 24, 16, 0, tzinfo=config.TIMEZONE),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 25, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 5, 25, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 5, 25, 16, 0, tzinfo=config.TIMEZONE),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 26, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 5, 26, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 5, 26, 16, 0, tzinfo=config.TIMEZONE),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 27, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 5, 27, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 5, 27, 16, 0, tzinfo=config.TIMEZONE),
            ),
        ]

        friday_calendar_days = [
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 27, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 5, 27, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 5, 27, 16, 0, tzinfo=config.TIMEZONE),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 30, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 5, 30, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 5, 30, 16, 0, tzinfo=config.TIMEZONE),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 5, 31, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 5, 31, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 5, 31, 16, 0, tzinfo=config.TIMEZONE),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 6, 1, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 6, 1, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 6, 1, 16, 0, tzinfo=config.TIMEZONE),
            ),
            MockAlpacaCalendar(
                date_value=datetime.date(1977, 6, 2, tzinfo=config.TIMEZONE),
                open_value=datetime.datetime(1977, 6, 2, 9, 30, tzinfo=config.TIMEZONE),
                close_value=datetime.datetime(1977, 6, 2, 16, 0, tzinfo=config.TIMEZONE),
            ),
        ]

        tests = [
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 9, 15, tzinfo=config.TIMEZONE),
                "is_market_open": False,
                "calendar_days": monday_calendar_days,
                "all_positions": [],
                "result": False,
            },
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 26, 9, 45, tzinfo=config.TIMEZONE),
                "is_market_open": True,
                "calendar_days": [
                    MockAlpacaCalendar(
                        date_value=datetime.date(1977, 5, 26, tzinfo=config.TIMEZONE),
                        open_value=datetime.datetime(1977, 5, 26, 9, 30, tzinfo=config.TIMEZONE),
                        close_value=datetime.datetime(1977, 5, 26, 16, 0, tzinfo=config.TIMEZONE),
                    ),
                ]
                + friday_calendar_days[1:],
                "all_positions": [],
                "result": True,
            },
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 27, 15, 30, tzinfo=config.TIMEZONE),
                "is_market_open": True,
                "calendar_days": friday_calendar_days,
                "all_positions": [],
                "result": False,
            },
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 9, 45, tzinfo=config.TIMEZONE),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [{}],
                "result": False,
            },
            {
                "action": trade.CREATE_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 9, 45, tzinfo=config.TIMEZONE),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [],
                "result": True,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 16, 15, tzinfo=config.TIMEZONE),
                "is_market_open": False,
                "calendar_days": monday_calendar_days,
                "all_positions": [],
                "result": False,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 26, 15, 30, tzinfo=config.TIMEZONE),
                "is_market_open": True,
                "calendar_days": [
                    MockAlpacaCalendar(
                        date_value=datetime.date(1977, 5, 26, tzinfo=config.TIMEZONE),
                        open_value=datetime.datetime(1977, 5, 26, 9, 30, tzinfo=config.TIMEZONE),
                        close_value=datetime.datetime(1977, 5, 26, 16, 0, tzinfo=config.TIMEZONE),
                    ),
                ]
                + friday_calendar_days[1:],
                "all_positions": [{}],
                "result": True,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 15, 30, tzinfo=config.TIMEZONE),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [{}],
                "result": False,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 23, 15, 30, tzinfo=config.TIMEZONE),
                "is_market_open": True,
                "calendar_days": monday_calendar_days,
                "all_positions": [],
                "result": False,
            },
            {
                "action": trade.CLEAR_ACTION,
                "current_datetime": datetime.datetime(1977, 5, 27, 15, 30, tzinfo=config.TIMEZONE),
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

            assert test["result"] == result


class TestPrivateGetAvailableTickers(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARQUBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
        )

    def tearDown(self) -> None:
        pass

    def test__get_available_tickers_darqube_get_tickers_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=None,
            exception=Exception("darqube get tickers error"),
        )

        with pytest.raises(Exception) as context:
            self.client._get_available_tickers()

        assert str(context.exception) == "darqube get tickers error"

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

        with pytest.raises(Exception) as context:
            self.client._get_available_tickers()

        assert str(context.exception) == "alpaca get all assets error"

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

        assert len(tickers) == 1
        assert tickers[0] == "TICKER"


class TestGetAvailableTickers(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARUQBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
        )

    def tearDown(self) -> None:
        pass

    def test_get_available_tickers_error(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_error

        with pytest.raises(Exception) as context:
            self.client.get_available_tickers()

        assert str(context.exception) == "get available tickers error"

    def test_get_available_tickers_success(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_success

        tickers = self.client.get_available_tickers()

        assert len(tickers) == 1
        assert tickers[0] == "TICKER"


class TestSetPositions(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARQUBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
        )

    def tearDown(self) -> None:
        pass

    def test_set_positions_get_available_tickers_error(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_error

        with pytest.raises(Exception) as context:
            self.client.set_positions(
                tickers=["TICKER"],
            )

        assert str(context.exception) == "get available tickers error"

    def test_set_positions_get_account_error(self) -> None:
        self.client._get_available_tickers = mock_get_available_tickers_success

        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions={
                "get_account": Exception("alpaca get account error"),
            },
        )

        with pytest.raises(Exception) as context:
            self.client.set_positions(
                tickers=["TICKER"],
            )

        assert str(context.exception) == "alpaca get account error"

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

        with pytest.raises(Exception) as context:
            self.client.set_positions(
                tickers=["TICKER"],
            )

        assert str(context.exception) == "alpaca submit order error"

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
        assert last_request is not None
        assert last_request.symbol == "TICKER"
        assert last_request.notional == 95.0
        assert last_request.side == "buy"


class TestClearPositions(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARUQBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
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

        with pytest.raises(Exception) as context:
            self.client.clear_positions()

        assert str(context.exception) == "alpaca close all positions error"

    def test_clear_positions_success(self) -> None:
        self.client.alpaca_trading_client = MockAlpacaTradingClient(
            responses=None,
            exceptions=None,
        )

        self.client.clear_positions()

        assert True  # TODO: needs test


class TestPrivateGetPortfolioDailyReturns(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARQUBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
        )

    def tearDown(self) -> None:
        pass

    def test__get_portfolio_daily_returns_http_client_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=None,
            exception=Exception("alpaca get portfolio returns error"),
        )

        with pytest.raises(Exception) as context:
            self.client._get_portoflio_daily_returns(
                week_count=1,
                end_at=datetime.datetime.now(tz=config.TIMEZONE),
            )

        assert str(context.exception) == "alpaca get portfolio returns error"

    def test__get_portfolio_daily_returns_insufficient_data_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=MockHTTPResponse(
                data={"timestamp": []},
            ),
            exception=None,
        )

        with pytest.raises(Exception) as context:
            self.client._get_portoflio_daily_returns(
                week_count=1,
                end_at=datetime.datetime.now(tz=config.TIMEZONE),
            )

        assert str(context.exception) == "insufficient portfolio data"

    def test__get_portfolio_daily_returns_success(self) -> Non:
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
            end_at=datetime.datetime.now(tz=config.TIMEZONE),
        )

        assert len(returns) == 5
        assert returns[0] == -0.0082


class TestPrivateGetBenchmarkDailyReturns(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARQUBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
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

        with pytest.raises(Exception) as context:
            self.client._get_benchmark_daily_returns(
                week_count=1,
                end_at=datetime.datetime.now(tz=config.TIMEZONE),
            )

        assert str(context.exception) == "alpaca get benchmark returns error"

    def test__get_benchmark_daily_returns_insufficient_data_error(self) -> None:
        self.client.alpaca_historical_client = MockAlpacaHistoricalClient(
            responses={
                "get_stock_bars": {
                    "SPY": [],
                },
            },
            exceptions=None,
        )

        with pytest.raises(Exception) as context:
            self.client._get_benchmark_daily_returns(
                week_count=1,
                end_at=datetime.datetime.now(tz=config.TIMEZONE),
            )

        assert str(context.exception) == "insufficient benchmark data"

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
            end_at=datetime.datetime.now(tz=config.TIMEZONE),
        )

        assert len(returns) == 5
        assert returns == [0.01, 0.0099, 0.0098, 0.0097, 0.0096]


class TestPrivateCumulativeReturns(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARQUBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
        )

    def tearDown(self) -> None:
        pass

    def test__cumulative_returns_success(self) -> None:
        returns = [0.01, 0.0099, 0.0098, 0.0097, 0.0096]

        cumulative_returns = self.client._cumulative_returns(
            returns=returns,
        )

        assert cumulative_returns == 0.05


class TestPrivateGetRiskFreeRate(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARQUBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
        )

    def tearDown(self) -> None:
        pass

    def test__get_risk_free_rate_http_client_error(self) -> None:
        self.client.http_client = MockHTTPClient(
            response=None,
            exception=Exception("get risk free rate error"),
        )

        with pytest.raises(Exception) as context:
            self.client._get_risk_free_rate()

        assert str(context.exception) == "get risk free rate error"

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

        assert risk_free_rate == 0.03


class TestGetPerformanceMetrics(unittest.TestCase):
    def setUp(self) -> None:
        self.client = trade.Client(
            darqube_api_key=DARQUBE_API_KEY,
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            alpha_vantage_api_key=ALPHA_VANTAGE_API_KEY,
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

        with pytest.raises(Exception) as context:
            self.client.get_performance_metrics(
                week_count=1,
                end_at=datetime.datetime.now(tz=config.TIMEZONE),
            )

        assert str(context.exception) == "alpaca get account error"

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

        with pytest.raises(Exception) as context:
            self.client.get_performance_metrics(
                week_count=1,
                end_at=datetime.datetime.now(tz=config.TIMEZONE),
            )

        assert str(context.exception) == "get portfolio returns error"

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

        with pytest.raises(Exception) as context:
            self.client.get_performance_metrics(
                week_count=1,
                end_at=datetime.datetime.now(tz=config.TIMEZONE),
            )

        assert str(context.exception) == "get benchmark returns error"

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

        with pytest.raises(Exception) as context:
            self.client.get_performance_metrics(
                week_count=1,
                end_at=datetime.datetime.now(tz=config.TIMEZONE),
            )

        assert str(context.exception) == "get risk free rate error"

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
            end_at=datetime.datetime.now(tz=config.TIMEZONE),
        )

        assert metrics["current_portfolio_value"] == 100.0
        assert metrics["cumulative_portfolio_returns"] == 0.0049
        assert metrics["cumulative_benchmark_returns"] == 0.05
        assert metrics["risk_free_rate"] == 0.03
        assert metrics["risk_free_rate"] == 0.03
