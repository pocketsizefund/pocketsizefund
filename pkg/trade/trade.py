import requests

from alpaca.data import requests as alpaca_data_requests
from alpaca.data import historical
from alpaca.trading import client as trading_client
from alpaca.trading import enums
from alpaca.trading import requests as alpaca_trading_requests


SIDE_BUY = 'buy'
SIDE_SELL = 'sell'


class Client:
    def __init__(
        self,
        darqube_api_key: str,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        is_paper: bool = True,
    ) -> None:
        self.darqube_api_key = darqube_api_key
        self.http_client = requests
        self.alpaca_trading_client = trading_client.TradingClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
            paper=is_paper,
        )
        self.alpaca_data_client = historical.StockHistoricalDataClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
        )

    def get_market_status(self) -> dict[str, any]:
        clock = self.alpaca_trading_client.get_clock()
        return {
            'is_market_open': clock.is_open,
        }

    def get_available_tickers(self) -> list[str]:
        # GSPC is the S&P 500
        darqube_response = self.http_client.get(
            url='https://api.darqube.com/data-api/fundamentals/indexes/index_constituents/GSPC',
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

    def get_available_cash(self) -> float:
        account = self.alpaca_trading_client.get_account()

        return float(account.cash)

    def get_current_prices(
        self,
        tickers: list[str],
    ) -> dict[str, float]:
        request = alpaca_data_requests.StockLatestTradeRequest(
            symbol_or_symbols=tickers,
        )

        response = self.alpaca_data_client.get_stock_latest_trade(request)

        prices: dict[str, dict[str, float]] = {
            ticker: {
                'price': float(response[ticker].price),
            } for ticker in response
        }

        return prices

    def set_positions(
        self,
        positions: list[dict[str, any]],
    ):
        pos_set = {}
        available_tickers = self.get_available_tickers()

        for position in positions:
            side: enums.OrderSide = None

            if position['side'] == SIDE_BUY:
                side = enums.OrderSide.BUY

            elif position['side'] == SIDE_SELL:
                side = enums.OrderSide.SELL

            quantity = position['quantity']
            if quantity <= 0:
                raise Exception('Quantity must be a positive number.')

            if position['ticker'] not in available_tickers:
                raise Exception(f"Invalid ticker: {position['ticker']}")

            
            request = alpaca_trading_requests.MarketOrderRequest(
                symbol=position['ticker'],
                qty=round(position['quantity'], 2),
                type=enums.OrderType.MARKET,
                side=side,
                time_in_force=enums.TimeInForce.DAY,
            )

            pos_set[position['ticker']] = self.alpaca_trading_client.submit_order(request)
        return pos_set


    def get_positions(self) -> dict[str, float]:
        portfolio = self.alpaca_trading_client.get_all_positions()
        port = {}

        for position in portfolio:
            port[position.symbol] = position.qty

        return port

    def clear_positions(self) -> None:
        self.alpaca_trading_client.close_all_positions(
            cancel_orders=True,
        )
