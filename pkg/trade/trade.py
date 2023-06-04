from alpaca.trading import client
from alpaca.trading import enums
from alpaca.trading import requests as alpaca_trading_requests
import finnhub


class Client:
    def __init__(
        self,
        finnhub_api_key: str,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        is_paper: bool = True,
    ) -> None:
        self.finnhub_client = finnhub.Client(api_key=finnhub_api_key)

        self.alpaca_trading_client = client.TradingClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
            paper=is_paper,
        )

    def get_available_tickers(self) -> list[str]:
        # GSPC is the S&P 500
        finnhub_response = self.finnhub_client.indices_const(symbol="^GSPC")

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
                asset.symbol in finnhub_response['constituents'] and
                '.' not in asset.symbol
            ):
                tickers.append(asset.symbol)

        return tickers


client = Client(
    finnhub_api_key='cgoce2pr01qpst9t98lgcgoce2pr01qpst9t98m0',
    alpaca_api_key='PK6T1U5I6S00I86PVW96',
    alpaca_api_secret='FBxHpTaIrQlSwK80Jl4yv8yacEcxN1Xwdf9L6qkV',
)

tickers = client.get_available_tickers()

print('count:', len(tickers))
print('tickers:', tickers)
