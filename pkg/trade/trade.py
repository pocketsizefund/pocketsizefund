import requests
from alpaca.trading import client as trading_client
from alpaca.trading import enums
from alpaca.trading import requests as alpaca_trading_requests


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

    def is_market_open(self) -> bool:
        clock = self.alpaca_trading_client.get_clock()

        return clock.is_open

    def get_available_tickers(self) -> list[str]:
        return self._get_available_tickers()

    def set_positions(
        self,
        tickers: list[str],
    ) -> None:
        available_tickers = self._get_available_tickers()

        account = self.alpaca_trading_client.get_account()

        available_cash = float(account.cash)

        notional = available_cash / len(tickers)

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
