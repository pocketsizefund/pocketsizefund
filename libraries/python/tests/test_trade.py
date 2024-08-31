import datetime
from typing import Optional

import pytest

from pocketsizefund import config, trade

client = trade.Client(
    darqube_api_key="darqube_api_key",
    alpaca_api_key="alpaca_api_key",
    alpaca_api_secret="alpaca_api_secret",  # noqa: S106
    alpha_vantage_api_key="alpha_vantage_api_key",
)


class MockAlpacaClock:
    def __init__(
        self,
        is_open: bool,  # noqa: FBT001
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
        headers: Optional[any] = None,  # noqa: UP007
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
        cancel_orders: bool,  # noqa: FBT001
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
    _ = week_count, end_at
    msg = "get portfolio returns error"
    raise ValueError(msg)


def mock_get_portfolio_returns_success(
    week_count: int,
    end_at: datetime.datetime,
) -> list[dict[str, any]]:
    _ = week_count, end_at
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
    _ = week_count, end_at
    msg = "get benchmark returns error"
    raise ValueError(msg)


def mock_get_benchmark_daily_returns_success(
    week_count: int,
    end_at: datetime.datetime,
) -> list[dict[str, any]]:
    _ = week_count, end_at
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


monday_calendar_days = [
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 5, 23),
            open_value=datetime.datetime(1977, 5, 23, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 5, 23, 16, 0, tzinfo=config.TIMEZONE),
        ),
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 5, 24),
            open_value=datetime.datetime(1977, 5, 24, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 5, 24, 16, 0, tzinfo=config.TIMEZONE),
        ),
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 5, 25),
            open_value=datetime.datetime(1977, 5, 25, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 5, 25, 16, 0, tzinfo=config.TIMEZONE),
        ),
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 5, 26),
            open_value=datetime.datetime(1977, 5, 26, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 5, 26, 16, 0, tzinfo=config.TIMEZONE),
        ),
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 5, 27),
            open_value=datetime.datetime(1977, 5, 27, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 5, 27, 16, 0, tzinfo=config.TIMEZONE),
        ),
    ]

friday_calendar_days = [
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 5, 27),
            open_value=datetime.datetime(1977, 5, 27, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 5, 27, 16, 0, tzinfo=config.TIMEZONE),
        ),
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 5, 30),
            open_value=datetime.datetime(1977, 5, 30, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 5, 30, 16, 0, tzinfo=config.TIMEZONE),
        ),
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 5, 31),
            open_value=datetime.datetime(1977, 5, 31, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 5, 31, 16, 0, tzinfo=config.TIMEZONE),
        ),
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 6, 1),
            open_value=datetime.datetime(1977, 6, 1, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 6, 1, 16, 0, tzinfo=config.TIMEZONE),
        ),
        MockAlpacaCalendar(
            date_value=datetime.date(1977, 6, 2),
            open_value=datetime.datetime(1977, 6, 2, 9, 30, tzinfo=config.TIMEZONE),
            close_value=datetime.datetime(1977, 6, 2, 16, 0, tzinfo=config.TIMEZONE),
        ),
    ]

@pytest.mark.parametrize("action, expected",
    [
        ({
            "action": trade.CREATE_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 23, 9, 15, tzinfo=config.TIMEZONE),
            "is_market_open": False,
            "calendar_days": monday_calendar_days,
            "all_positions": [],
        },  False),
        ({
            "action": trade.CREATE_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 26, 9, 45, tzinfo=config.TIMEZONE),
            "is_market_open": True,
            "calendar_days": [
                MockAlpacaCalendar(
                    date_value=datetime.date(1977, 5, 26),
                    open_value=datetime.datetime(1977, 5, 26, 9, 30, tzinfo=config.TIMEZONE),
                    close_value=datetime.datetime(1977, 5, 26, 16, 0, tzinfo=config.TIMEZONE),
                ),
            ]
            + friday_calendar_days[1:],
            "all_positions": [],
        }, True),
        ({
            "action": trade.CREATE_ACTION,

            "current_datetime": datetime.datetime(1977, 5, 27, 15, 30, tzinfo=config.TIMEZONE),
            "is_market_open": True,
            "calendar_days": friday_calendar_days,
            "all_positions": [],
        }, False),
        ({
            "action": trade.CREATE_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 23, 9, 45, tzinfo=config.TIMEZONE),
            "is_market_open": True,
            "calendar_days": monday_calendar_days,
            "all_positions": [{}],
        }, False),
        ({
            "action": trade.CREATE_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 23, 9, 45, tzinfo=config.TIMEZONE),
            "is_market_open": True,
            "calendar_days": monday_calendar_days,
            "all_positions": [],
        }, True),
        ({
            "action": trade.CLEAR_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 23, 16, 15, tzinfo=config.TIMEZONE),
            "is_market_open": False,
            "calendar_days": monday_calendar_days,
            "all_positions": [],
        }, False),
        ({
            "action": trade.CLEAR_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 26, 15, 30, tzinfo=config.TIMEZONE),
            "is_market_open": True,
            "calendar_days": [
                MockAlpacaCalendar(
                    date_value=datetime.date(1977, 5, 26),
                    open_value=datetime.datetime(1977, 5, 26, 9, 30, tzinfo=config.TIMEZONE),
                    close_value=datetime.datetime(1977, 5, 26, 16, 0, tzinfo=config.TIMEZONE),
                ),
            ]
            + friday_calendar_days[1:],
            "all_positions": [{}],
        }, True),
        ({
            "action": trade.CLEAR_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 23, 15, 30, tzinfo=config.TIMEZONE),
            "is_market_open": True,
            "calendar_days": monday_calendar_days,
            "all_positions": [{}],
        }, False),
        ({
            "action": trade.CLEAR_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 23, 15, 30, tzinfo=config.TIMEZONE),
            "is_market_open": True,
            "calendar_days": monday_calendar_days,
            "all_positions": [],
        }, False),
        ({
            "action": trade.CLEAR_ACTION,
            "current_datetime": datetime.datetime(1977, 5, 27, 15, 30, tzinfo=config.TIMEZONE),
            "is_market_open": True,
            "calendar_days": monday_calendar_days,
            "all_positions": [{}],
        }, True),
    ])
def test_check_position_set_availability_success(action: dict, expected: bool) -> None: # noqa: FBT001
    client.alpaca_trading_client = MockAlpacaTradingClient(
        responses={
            "get_clock": MockAlpacaClock(
                is_open=action["is_market_open"],
            ),
            "get_calendar": action["calendar_days"],
            "get_all_positions": action["all_positions"],
        },
        exceptions=None,
    )

    def mock_get_next_trading_day(current_date: datetime.datetime) -> None:
        for day in action["calendar_days"]:
            if day.date > current_date.date():
                return day.date
        return None

    client.get_next_trading_day = mock_get_next_trading_day

    result = client.check_set_position_availability(
        action=action["action"],
        current_datetime=action["current_datetime"],
    )

    assert result == expected


def test__get_available_tickers_darqube_get_tickers_error() -> None:
    client.http_client = MockHTTPClient(
        response=None,
        exception=Exception("darqube get tickers error"),
    )

    with pytest.raises(Exception) as context:
        client._get_available_tickers()

    assert str(context.value) == "darqube get tickers error"


def test__get_available_tickers_alpaca_get_all_assets_error() -> None:
    client.http_client = MockHTTPClient(
        response=MockHTTPResponse(
            data={
                "0": {
                    "Code": "TICKER",
                },
            },
        ),
        exception=None,
    )

    client.alpaca_trading_client = MockAlpacaTradingClient(
        responses=None,
        exceptions={
            "get_all_assets": Exception("alpaca get all assets error"),
        },
    )

    with pytest.raises(Exception) as context:
        client._get_available_tickers()

    assert str(context.value) == "alpaca get all assets error"


def test__get_available_tickers_success() -> None:
    client.http_client = MockHTTPClient(
        response=MockHTTPResponse(
            data={
                "0": {
                    "Code": "TICKER",
                },
            },
        ),
        exception=None,
    )

    client.alpaca_trading_client = MockAlpacaTradingClient(
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

    tickers = client._get_available_tickers()

    assert len(tickers) == 1
    assert tickers[0] == "TICKER"


def test_get_available_tickers_error() -> None:
    client._get_available_tickers = mock_get_available_tickers_error

    with pytest.raises(Exception) as context:
        client.get_available_tickers()

    assert str(context.value) == "get available tickers error"


def test_get_available_tickers_success() -> None:
    client._get_available_tickers = mock_get_available_tickers_success

    tickers = client.get_available_tickers()

    assert len(tickers) == 1
    assert tickers[0] == "TICKER"
