"""Manage and execute trades."""

import datetime

import requests
from alpaca.data import historical
from alpaca.trading import client as trading_client
from alpaca.trading import enums
from alpaca.trading import requests as alpaca_trading_requests

CREATE_ACTION = "create"
CLEAR_ACTION = "clear"


class Client:
    def __init__(  # noqa: PLR0913
        self,
        darqube_api_key: str,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        alpha_vantage_api_key: str,
        is_paper: bool = True,  # noqa: FBT001, FBT002
    ) -> None:
        """Initialize a new instance of the Client class.

        This is used to interact with several systems including:
        - Darqube
        - Alpaca
        - Alpha Vantage

        Args:
            darqube_api_key (str): The API key for Darqube.
            alpaca_api_key (str): The API key for Alpaca.
            alpaca_api_secret (str): The API secret key for Alpaca.
            alpha_vantage_api_key (str): The API key for Alpha Vantage.
            is_paper (bool, optional): Indicates whether the client is in paper trading mode.
                Defaults to True.

        Returns:
            None
        """
        self.darqube_api_key = darqube_api_key
        self.http_client = requests
        self.http_headers = {
            "accept": "application/json",
            "APCA-API-KEY-ID": alpaca_api_key,
            "APCA-API-SECRET-KEY": alpaca_api_secret,
        }
        self.alpaca_trading_client = trading_client.TradingClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
            paper=is_paper,
        )
        self.alpaca_historical_client = historical.StockHistoricalDataClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
            raw_data=True,
        )
        self.alpha_vantage_api_key = alpha_vantage_api_key
        self.schedule_periods = (
            datetime.time(hour=9, minute=30),
            datetime.time(hour=11, minute=30),
            datetime.time(hour=14, minute=00),
            datetime.time(hour=16, minute=00),
        )
        self.is_paper = is_paper

    def check_set_position_availability(
        self,
        action: str,
        current_datetime: datetime.datetime,
    ) -> bool:
        """Check the availability of setting a position.

        Uses the clock from Alpaca.

        Args:
            action (str): The action to perform.
                Can be either "CREATE_ACTION" or "CLEAR_ACTION".
            current_datetime (datetime.datetime): The current datetime.

        Returns:
            bool: True if the position can be set, False otherwise.

        Raises:
            NotImplementedError: If the action is unknown.
        """
        clock = self.alpaca_trading_client.get_clock()

        if not clock.is_open:
            return False

        calendar_days = self.alpaca_trading_client.get_calendar(
            filters=alpaca_trading_requests.GetCalendarRequest(
                start=current_datetime.date(),
                end=(current_datetime + datetime.timedelta(days=5)).date(),
            ),
        )

        positions = self.alpaca_trading_client.get_all_positions()

        has_positions = len(positions) > 0

        calendar_days_in_week = [
            day
            for day in calendar_days
            if day.date.isocalendar()[1] == current_datetime.isocalendar()[1]
        ]

        last_day_in_week = max(calendar_days_in_week, key=lambda day: day.date)

        is_last_day = last_day_in_week.date == current_datetime.date()

        last_period_in_day = ()

        for index in range(len(self.schedule_periods) - 1):
            start = self.schedule_periods[index]
            end = self.schedule_periods[index + 1]
            close = last_day_in_week.close.time()

            if close >= start and close <= end:
                last_period_in_day = (start, end)
                break

        is_last_period = (
            last_period_in_day[0] <= current_datetime.time()
            and last_period_in_day[1] >= current_datetime.time()
        )

        if action == CREATE_ACTION:
            if has_positions or is_last_day and is_last_period:
                return False

            return True

        if action == CLEAR_ACTION:
            if has_positions and is_last_day and is_last_period:
                return True

            return False

        msg = f"unknown {action=}"
        raise NotImplementedError(msg)

    def get_available_tickers(self) -> list[str]:
        """Retrieve a list of available tickers.

        Returns:
            list[str]: A list of strings representing the available tickers.

        """
        return self._get_available_tickers()

    def baseline_buy(
        self,
        ticker: str,
    ) -> None:
        """Buy a $1 position in the provided ticker.

        This is just being added in the interest of getting things up and
        running but will be deprecated after swapping over to the Rust implementation.
        """
        available_tickers = self._get_available_tickers()

        if ticker not in available_tickers:
            msg = f'invalid ticker "{ticker}"'
            raise ValueError(msg)

        request = alpaca_trading_requests.MarketOrderRequest(
            symbol=ticker,
            notional=round(1, 2),
            type=enums.OrderType.MARKET,
            side=enums.OrderSide.BUY,
            time_in_force=enums.TimeInForce.DAY,
        )

        self.alpaca_trading_client.submit_order(request)

    def _get_available_tickers(self) -> list[str]:
        """Retrieve a list of available tickers from Darqube API and Alpaca Trading API.

        `GSPC` is the S&P 500 Index
        `DJI` is the Dow Jones Industrial Average

        Returns:
            A list of strings representing the available tickers.
        """
        darqube_response = self.http_client.get(
            url="https://api.darqube.com/data-api/fundamentals/indexes/index_constituents/DJI",
            params={
                "token": self.darqube_api_key,
            },
        )

        darqube_response_json = darqube_response.json()

        constituents = [darqube_response_json[key]["Code"] for key in darqube_response_json]

        request = alpaca_trading_requests.GetAssetsRequest(
            status=enums.AssetStatus.ACTIVE,
            asset_class=enums.AssetClass.US_EQUITY,
        )

        alpaca_response = self.alpaca_trading_client.get_all_assets(request)

        tickers: list[str] = []
        for asset in alpaca_response:
            if (
                asset.tradable
                and asset.fractionable
                and asset.shortable
                and asset.symbol in constituents
                and "." not in asset.symbol
            ):
                tickers.append(asset.symbol)  # noqa: PERF401

        return tickers
