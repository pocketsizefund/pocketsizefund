import datetime

import requests
from alpaca.trading import client as trading_client
from alpaca.trading import enums
from alpaca.trading import requests as alpaca_trading_requests
from alpaca.data import historical
from alpaca.data import timeframe
from alpaca.data import requests as alpaca_data_requests
import numpy
import boto3


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
            'accept': 'application/json',
            'APCA-API-KEY-ID': alpaca_api_key,
            'APCA-API-SECRET-KEY': alpaca_api_secret,
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
        self.event_bridge_client = boto3.client('scheduler')
        self.create_positions_schedule_name = 'pocketsizefund-create-positions'
        self.clear_positions_schedule_name = 'pocketsizefund-clear-positions'
        self.is_paper = is_paper

    def is_market_open(self) -> bool:
        clock = self.alpaca_trading_client.get_clock()

        is_market_open = clock.is_open

        return is_market_open

    def set_position_schedules(
        self,
        start_at: datetime.datetime,
        create_positions_lambda_arn: str,
        clear_positions_lambda_arn: str,
        invoke_lambda_role_arn: str,
    ) -> None:
        response = self.event_bridge_client.list_schedules(
            NamePrefix='pocketsizefund-',
            State='ENABLED',
        )

        for schedule in response['Schedules']:
            name = schedule['Name']
            if name in [
                self.create_positions_schedule_name,
                self.clear_positions_schedule_name,
            ]:
                self.event_bridge_client.delete_schedule(Name=name)

        days_until_saturday = (5 - start_at.weekday() + 7) % 7

        next_saturday = start_at + datetime.timedelta(days=days_until_saturday)

        calendar_days = self.alpaca_trading_client.get_calendar(
            filters=alpaca_trading_requests.GetCalendarRequest(
                start=start_at,
                end=start_at + datetime.timedelta(days=7),
            )
        )

        filtered_days = [
            calendar_day for calendar_day in calendar_days
            if calendar_day.date < next_saturday.date()
            and calendar_day.date > start_at.date()
        ]

        create_open = min(filtered_days, key=lambda x: x.date).open + datetime.timedelta(
            minutes=30,
        )

        create_expression = 'at({})'.format(
            create_open.strftime("%Y-%m-%dT%H:%M:%S")
        )

        self._create_schedule(
            name=self.create_positions_schedule_name,
            expression=create_expression,
            lambda_arn=create_positions_lambda_arn,
            role_arn=invoke_lambda_role_arn,
        )

        clear_close = max(filtered_days, key=lambda x: x.date).close - datetime.timedelta(
            minutes=30,
        )

        clear_expression = 'at({})'.format(
            clear_close.strftime("%Y-%m-%dT%H:%M:%S")
        )

        self._create_schedule(
            name=self.clear_positions_schedule_name,
            expression=clear_expression,
            lambda_arn=clear_positions_lambda_arn,
            role_arn=invoke_lambda_role_arn,
        )

    def _create_schedule(
        self,
        name: str,
        expression: str,
        lambda_arn: str,
        role_arn: str,
    ) -> None:
        self.event_bridge_client.create_schedule(
            Description='Dynamically generated schedule for managing positions',
            ScheduleExpression=expression,
            ScheduleExpressionTimezone='America/New_York',
            Name=name,
            State='ENABLED',
            FlexibleTimeWindow={
                'Mode': 'OFF',
            },
            Target={
                'Arn': lambda_arn,
                'RoleArn': role_arn,
            },
        )

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
                raise Exception('invalid ticker "{}"'.format(ticker))

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
            url='https://api.darqube.com/data-api/fundamentals/indexes/index_constituents/DJI',
            params={
                'token': self.darqube_api_key,
            },
        )

        darqube_response_json = darqube_response.json()

        constituents = [
            darqube_response_json[key]['Code']
            for key in darqube_response_json
        ]

        request = alpaca_trading_requests.GetAssetsRequest(
            status=enums.AssetStatus.ACTIVE,
            asset_class=enums.AssetClass.US_EQUITY,
        )

        alpaca_response = self.alpaca_trading_client.get_all_assets(request)

        tickers: list[str] = []
        for asset in alpaca_response:
            if (
                asset.tradable and
                asset.fractionable and
                asset.shortable and
                asset.symbol in constituents and
                '.' not in asset.symbol
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

        metrics['current_portfolio_value'] = float(account.equity)

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
            len(portfolio_returns) - adjusted_length:
        ]

        benchmark_returns = benchmark_returns[
            len(benchmark_returns) - adjusted_length:
        ]

        metrics['cumulative_portfolio_returns'] = self._cumulative_returns(
            returns=portfolio_returns,
        )

        metrics['cumulative_benchmark_returns'] = self._cumulative_returns(
            returns=benchmark_returns,
        )

        metrics['risk_free_rate'] = self._get_risk_free_rate()

        return metrics

    def _get_portoflio_daily_returns(
        self,
        week_count: int,
        end_at: datetime.datetime,
    ) -> list[dict[str, any]]:
        subdomain = 'paper-api'
        if not self.is_paper:
            subdomain = 'api'

        portfolio_response = self.http_client.get(
            url='https://{}.alpaca.markets/v2/account/portfolio/history'.format(
                subdomain,
            ),
            headers=self.http_headers,
            params={
                'period': '{}W'.format(week_count),
                'timeframe': '1D',
                'intraday_reporting': 'market_hours',
                'pnl_reset': 'per_day',
                'end': end_at.strftime('%Y-%m-%dT%H:%M:%SZ'),
            },
        )

        portfolio_data = portfolio_response.json()

        portfolio_returns = []

        for index in range(len(portfolio_data['timestamp'])):
            portfolio_returns.append(
                round(float(portfolio_data['profit_loss_pct'][index]), 4)
            )

        if len(portfolio_returns) < 5:
            raise Exception('insufficient portfolio data')

        return portfolio_returns

    def _get_benchmark_daily_returns(
        self,
        week_count: int,
        end_at: datetime.datetime,
    ) -> list[dict[str, any]]:
        benchmark_ticker = 'SPY'

        # calendar days approximating trading days
        start_at = end_at - datetime.timedelta(days=week_count * 8)

        request = alpaca_data_requests.StockBarsRequest(
            symbol_or_symbols=benchmark_ticker,
            start=start_at,
            end=end_at,
            timeframe=timeframe.TimeFrame.Day,
            adjustment='all',
        )

        benchmark_response = self.alpaca_historical_client.get_stock_bars(
            request
        )

        benchmark_data = benchmark_response[benchmark_ticker]

        benchmark_returns = []

        for index in range(len(benchmark_data) - 1):
            current_close = float(benchmark_data[index]['c'])

            next_close = float(benchmark_data[index + 1]['c'])

            percent_change = (next_close / current_close) - 1

            benchmark_returns.append(round(percent_change, 4))

        if len(benchmark_returns) < 5:
            raise Exception('insufficient benchmark data')

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
            url='https://www.alphavantage.co/query',
            params={
                'function': 'TREASURY_YIELD',
                'interval': 'monthly',
                'maturity': '10year',
                'apikey': self.alpha_vantage_api_key,
            },
        )

        treasury_yields_data = treasury_yields_response.json()

        treasury_yields_data_sorted = sorted(
            treasury_yields_data['data'],
            key=lambda x: x['date'],
            reverse=True,
        )

        risk_free_rate = float(treasury_yields_data_sorted[0]['value']) * 0.01

        return risk_free_rate
