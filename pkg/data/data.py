import time

import requests


# Slightly more restrictive than the default
# 5 requests per minute
ALPHA_VANTAGE_DELAY_IN_SECONDS = 15


class Client:
    def __init__(
        self,
        alpha_vantage_api_key: str,
    ) -> None:
        self.alpha_vantage_api_key = alpha_vantage_api_key

    def get_equities_raw_bars(
        self,
        tickers: list[str],
    ) -> list[dict[any, any]]:
        equities_bars: list[dict[any, any]] = []
        for ticker in tickers:
            equity_bars = self.get_equity_raw_bars(ticker=ticker)
            equities_bars.append(equity_bars)
            time.sleep(ALPHA_VANTAGE_DELAY_IN_SECONDS)

        return equities_bars

    def get_equity_raw_bars(
        self,
        ticker: str,
    ) -> dict[any, any]:
        response = requests.get(
            url='https://www.alphavantage.co/query',
            params={
                'function': 'TIME_SERIES_DAILY_ADJUSTED',
                'symbol': ticker,
                'outputsize': 'full',
                'apikey': self.alpha_vantage_api_key,
            })

        return response.json()
