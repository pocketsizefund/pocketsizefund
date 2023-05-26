from alpaca.trading import client
from alpaca.trading import enums
from alpaca.trading import requests as alpaca_trading_requests


class Client:
    def __init__(
        self,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        is_paper: bool = True,
    ) -> None:
        self.alpaca_trading_client = client.TradingClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
            paper=is_paper,
        )

    def get_available_tickers(self) -> list[str]:
        request = alpaca_trading_requests.GetAssetsRequest(
            status=enums.AssetStatus.ACTIVE,
            asset_class=enums.AssetClass.US_EQUITY,
        )

        response = self.alpaca_trading_client.get_all_assets(request)

        tickers: list[str] = []
        for asset in response:
            if (
                asset.tradable and
                asset.fractionable and
                asset.shortable
            ):
                tickers.append(asset.symbol)

        return tickers
