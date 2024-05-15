import datetime

import numpy
import requests
from alpaca.data import historical
from alpaca.data import requests as alpaca_data_requests
from alpaca.data import timeframe
from alpaca.trading import client as trading_client
from alpaca.trading import enums
from alpaca.trading import requests as alpaca_trading_requests

CREATE_ACTION = "create"
CLEAR_ACTION = "clear"


class Client:
    def __init__(
        self,
        darqube_api_key: str,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        alpha_vantage_api_key: str,
        is_paper: bool = True,
    ) -> None:
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

        elif action == CLEAR_ACTION:
            if has_positions and is_last_day and is_last_period:
                return True

            return False

    def get_available_tickers(self) -> list[str]:
        return self._get_available_tickers()

    def set_positions(
        self,
        tickers: list[str],
    ) -> None:
        available_tickers = self._get_available_tickers()

        account = self.alpaca_trading_client.get_account()

        available_cash = float(account.cash)

        notional = available_cash / len(tickers) * 0.95  # 5% buffer workaround

        for ticker in tickers:
            if ticker not in available_tickers:
                raise Exception(f'invalid ticker "{ticker}"')

            request = alpaca_trading_requests.MarketOrderRequest(
                symbol=ticker,
                notional=round(notional, 2),
                type=enums.OrderType.MARKET,
                side=enums.OrderSide.BUY,
                time_in_force=enums.TimeInForce.DAY,
            )

            self.alpaca_trading_client.submit_order(request)

    def _get_available_tickers(self) -> list[str]:
        # "GSPC" is the S&P 500 Index
        # "DJI" is the Dow Jones Industrial Average
        darqube_response = self.http_client.get(
            url="https://api.darqube.com/data-api/fundamentals/indexes/index_constituents/DJI",
            params={
                "token": self.darqube_api_key,
            },
        )

        darqube_response_json = darqube_response.json()

        constituents = [
            darqube_response_json[key]["Code"] for key in darqube_response_json
        ]

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
                tickers.append(asset.symbol)

        return tickers

    def clear_positions(self) -> None:
        self.alpaca_trading_client.close_all_positions(
            cancel_orders=True,
        )

    def get_performance_metrics(
        self,
        week_count: int,
        end_at: datetime.datetime,
    ) -> dict[str, any]:
        metrics = {}

        account = self.alpaca_trading_client.get_account()

        metrics["current_portfolio_value"] = float(account.equity)

        portfolio_returns = self._get_portoflio_daily_returns(
            week_count=week_count,
            end_at=end_at,
        )

        benchmark_returns = self._get_benchmark_daily_returns(
            week_count=week_count,
            end_at=end_at,
        )

        adjusted_length = min(len(portfolio_returns), len(benchmark_returns))

        portfolio_returns = portfolio_returns[
            len(portfolio_returns) - adjusted_length :
        ]

        benchmark_returns = benchmark_returns[
            len(benchmark_returns) - adjusted_length :
        ]

        metrics["cumulative_portfolio_returns"] = self._cumulative_returns(
            returns=portfolio_returns,
        )

        metrics["cumulative_benchmark_returns"] = self._cumulative_returns(
            returns=benchmark_returns,
        )

        metrics["risk_free_rate"] = self._get_risk_free_rate()

        return metrics

    def _get_portoflio_daily_returns(
        self,
        week_count: int,
        end_at: datetime.datetime,
    ) -> list[dict[str, any]]:
        subdomain = "paper-api"
        if not self.is_paper:
            subdomain = "api"

        portfolio_response = self.http_client.get(
            url=f"https://{subdomain}.alpaca.markets/v2/account/portfolio/history",
            headers=self.http_headers,
            params={
                "period": f"{week_count}W",
                "timeframe": "1D",
                "intraday_reporting": "market_hours",
                "pnl_reset": "per_day",
                "end": end_at.strftime("%Y-%m-%dT%H:%M:%SZ"),
            },
        )

        portfolio_data = portfolio_response.json()

        portfolio_returns = []

        for index in range(len(portfolio_data["timestamp"])):
            portfolio_returns.append(
                round(float(portfolio_data["profit_loss_pct"][index]), 4),
            )

        if len(portfolio_returns) < 5:
            raise Exception("insufficient portfolio data")

        return portfolio_returns

    def _get_benchmark_daily_returns(
        self,
        week_count: int,
        end_at: datetime.datetime,
    ) -> list[dict[str, any]]:
        benchmark_ticker = "SPY"

        # adjusting due to Alpaca API limitations
        end_at = end_at - datetime.timedelta(hours=1)

        # calendar days approximating trading days
        start_at = end_at - datetime.timedelta(days=week_count * 8)

        request = alpaca_data_requests.StockBarsRequest(
            symbol_or_symbols=benchmark_ticker,
            start=start_at,
            end=end_at,
            timeframe=timeframe.TimeFrame.Day,
            adjustment="all",
        )

        benchmark_response = self.alpaca_historical_client.get_stock_bars(request)

        benchmark_data = benchmark_response[benchmark_ticker]

        benchmark_returns = []

        for index in range(len(benchmark_data) - 1):
            current_close = float(benchmark_data[index]["c"])

            next_close = float(benchmark_data[index + 1]["c"])

            percent_change = (next_close / current_close) - 1

            benchmark_returns.append(round(percent_change, 4))

        if len(benchmark_returns) < 5:
            raise Exception("insufficient benchmark data")

        return benchmark_returns

    def _cumulative_returns(
        self,
        returns: list[float],
    ) -> float:
        cumulative_returns = numpy.prod(1 + numpy.array(returns)) - 1

        return round(cumulative_returns, 4)

    def _get_risk_free_rate(
        self,
    ) -> float:
        treasury_yields_response = self.http_client.get(
            url="https://www.alphavantage.co/query",
            params={
                "function": "TREASURY_YIELD",
                "interval": "monthly",
                "maturity": "10year",
                "apikey": self.alpha_vantage_api_key,
            },
        )

        treasury_yields_data = treasury_yields_response.json()

        treasury_yields_data_sorted = sorted(
            treasury_yields_data["data"],
            key=lambda x: x["date"],
            reverse=True,
        )

        risk_free_rate = float(treasury_yields_data_sorted[0]["value"]) * 0.01

        return risk_free_rate
