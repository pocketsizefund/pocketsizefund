from __future__ import annotations  # noqa: I001
import pytest
from pocketsizefund import trade


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
        headers: any | None = None,  # noqa: UP007
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
        date_value: any,
        open_value: any,
        close_value: any,
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
        if (
            self.exceptions is not None
            and self.exceptions["get_all_assets"] is not None
        ):
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

        if (
            self.exceptions is not None
            and self.exceptions["close_all_positions"] is not None
        ):
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
        if (
            self.exceptions is not None
            and self.exceptions["get_stock_bars"] is not None
        ):
            raise self.exceptions["get_stock_bars"]

        return self.responses["get_stock_bars"]


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
