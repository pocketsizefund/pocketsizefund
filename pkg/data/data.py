import time
import datetime

import requests
import pandas

from pkg.data import bar

# Slightly more restrictive than the default
# 5 requests per minute
ALPHA_VANTAGE_DELAY_IN_SECONDS = 15


class Client:
    def __init__(
        self,
        alpha_vantage_api_key: str,
        log_progress: bool = False,
    ) -> None:
        self.alpha_vantage_api_key = alpha_vantage_api_key
        self.log_progress = log_progress

    def get_equities_raw_bars(
        self,
        tickers: list[str],
    ) -> list[dict[any, any]]:
        """
        get_equities_raw_bars is primarily used for backfilling
        """
        equities_bars: list[dict[any, any]] = []
        for ticker in tickers:
            if self.log_progress:
                print('getting {} bars'.format(ticker))
            equity_bars = self.get_equity_raw_bars(ticker=ticker)
            equities_bars.append(equity_bars)
            time.sleep(ALPHA_VANTAGE_DELAY_IN_SECONDS)

        return equities_bars

    def get_equity_raw_bars(
        self,
        ticker: str,
    ) -> dict[any, any]:
        """
        get_equity_raw_bars is primarily used for backfilling
        """
        response = requests.get(
            url='https://www.alphavantage.co/query',
            params={
                'function': 'TIME_SERIES_DAILY_ADJUSTED',
                'symbol': ticker,
                'outputsize': 'full',
                'apikey': self.alpha_vantage_api_key,
            })

        return response.json()

    def convert_equity_raw_bars_to_dataframe(
        self,
        equity_raw_bars: dict[any, any],
    ) -> pandas.DataFrame:
        """
        convert_equity_raw_bars_to_dataframe is primarily used
        for backfilling
        """
        daily_bars = equity_raw_bars['Time Series (Daily)']

        bars = [bar.Bar(
            timestamp=datetime.datetime.strptime(daily_bar[0], '%Y-%m-%d'),
            ticker=equity_raw_bars['Meta Data']['2. Symbol'],
            open_price=float(daily_bar[1]['1. open']),
            high_price=float(daily_bar[1]['2. high']),
            low_price=float(daily_bar[1]['3. low']),
            close_price=float(daily_bar[1]['5. adjusted close']),
            volume=float(daily_bar[1]['6. volume']),
            source=bar.SOURCE_ALPHA_VANTAGE,
        ) for daily_bar in daily_bars.items()]

        dataframe = pandas.DataFrame.from_dict(
            data=[bar.__dict__ for bar in bars],
        )

        return dataframe
