import requests

from alpaca.broker import client as broker_client
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
        alpaca_account_id: str,
        is_paper: bool = True,
    ) -> None:
        self.darqube_api_key = darqube_api_key
        self.http_client = requests
        self.alpaca_trading_client = trading_client.TradingClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
            paper=is_paper,
        )
        self.alpaca_broker_client = broker_client.BrokerClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
        )
        self.alpaca_data_client = historical.StockHistoricalDataClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
        )
        self.alpaca_account_id = alpaca_account_id

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
            darqube_response_json[key]["Code"]
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

    def get_available_cash(self) -> int:
        account = self.alpaca_trading_client.get_account()

        return int(account.cash)

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
    ) -> None:
        for position in positions:
            request = alpaca_trading_requests.MarketOrderRequest(
                symbol=position['ticker'],
                qty=position['quantity'],
                type=enums.OrderType.MARKET,
                time_in_force=enums.TimeInForce.DAY,
            )

            if position['side'] == SIDE_BUY:
                request['side'] = enums.OrderSide.BUY

            elif position['side'] == SIDE_SELL:
                request['side'] = enums.OrderSide.SELL

            self.alpaca_broker_client.submit_order_for_account(
                account_id=self.alpaca_account_id,
                order_data=request,
            )

    def clear_positions(self) -> None:
        self.alpaca_trading_client.close_all_positions()
