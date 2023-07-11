import time
import datetime

import requests
import pandas
from alpaca.data import historical
from alpaca.data import requests as alpaca_data_requests
from alpaca.data import timeframe


ALPHA_VANTAGE_DELAY_IN_SECONDS = 15  # within 5 requests/minute API limitation
ALPACA_TICKER_CHUNK_SIZE = 50
ALPACA_DATETIME_CHUNK_SIZE_IN_DAYS = 200
ALPACA_MAXIMUM_DAYS_IN_RANGE = 365 * 2  # Alpaca API limitation

SOURCE_ALPHA_VANTAGE = 'ALPHA_VANTAGE'
SOURCE_ALPACA = 'ALPACA'


class Client:
    def __init__(
        self,
        alpha_vantage_api_key: str,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        print_logs: bool = False,
    ) -> None:
        self.alpha_vantage_api_key = alpha_vantage_api_key
        self.alpaca_historical_client = historical.StockHistoricalDataClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
            raw_data=True,
        )
        self.http_client = requests
        self.print_logs = print_logs
        self.runtime_start = None

    def get_all_equities_bars(
        self,
        tickers: list[str],
    ) -> pandas.DataFrame:
        if self.print_logs:
            self.runtime_start = datetime.datetime.now()
            print('beginning get all data')

        bars: list[dict[str, any]] = []
        for ticker in tickers:
            if self.print_logs:
                print('getting {} bars'.format(ticker))

            response = self.http_client.get(
                url='https://www.alphavantage.co/query',
                params={
                    'function': 'TIME_SERIES_DAILY_ADJUSTED',
                    'symbol': ticker,
                    'outputsize': 'full',
                    'apikey': self.alpha_vantage_api_key,
                },
            )

            response_json = response.json()
            daily_bars = response_json['Time Series (Daily)']

            ticker_bars = [{
                'timestamp': datetime.datetime.strptime(daily_bar[0], '%Y-%m-%d'),
                'ticker': response_json['Meta Data']['2. Symbol'],
                'open_price': round(float(daily_bar[1]['1. open']), 2),
                'high_price': round(float(daily_bar[1]['2. high']), 2),
                'low_price': round(float(daily_bar[1]['3. low']), 2),
                'close_price': round(float(daily_bar[1]['5. adjusted close']), 2),
                'volume': round(float(daily_bar[1]['6. volume']), 2),
                'source': SOURCE_ALPHA_VANTAGE,
            } for daily_bar in daily_bars.items()]

            bars.extend(ticker_bars)

            time.sleep(ALPHA_VANTAGE_DELAY_IN_SECONDS)

        dataframe = pandas.DataFrame.from_dict(
            data=bars,
        )

        if self.print_logs:
            runtime_stop = datetime.datetime.now()

            runtime_in_minutes = (
                runtime_stop - self.runtime_start,
            ).total_seconds() / 60

            print('ending get all data')
            print('runtime {} minutes'.format(runtime_in_minutes))

        return dataframe

    def get_range_equities_bars(
        self,
        tickers: list[str],
        start_at: datetime.datetime,
        end_at: datetime.datetime,
    ) -> pandas.DataFrame:
        if self.print_logs:
            self.runtime_start = datetime.datetime.now()
            print('beginning get range data')

        # adjusted by 30 minutes to account for the
        # Alpaca free tier constraint
        end_at = end_at + datetime.timedelta(minutes=-30)

        start_at = start_at.replace(hour=0, minute=0, second=0)
        end_at = end_at.replace(hour=0, minute=0, second=0)

        difference_in_days = (end_at - start_at).days

        if difference_in_days > ALPACA_MAXIMUM_DAYS_IN_RANGE:
            raise Exception('range must be within {} days'.format(
                ALPACA_MAXIMUM_DAYS_IN_RANGE,
            ))

        bars: list[dict[str, any]] = []
        # chunking requests due to Alpaca request limitations
        for i in range(0, len(tickers), ALPACA_TICKER_CHUNK_SIZE):
            tickers_chunk = tickers[i:i+ALPACA_TICKER_CHUNK_SIZE]

            if self.print_logs:
                print('getting {} bars'.format(tickers_chunk))

            request: alpaca_data_requests.StockBarsRequest = None
            for j in range(0, difference_in_days, ALPACA_DATETIME_CHUNK_SIZE_IN_DAYS):
                start_at_chunk = start_at + datetime.timedelta(days=j)
                end_at_chunk = start_at_chunk + datetime.timedelta(
                    days=ALPACA_DATETIME_CHUNK_SIZE_IN_DAYS,
                )

                if end_at_chunk > end_at:
                    end_at_chunk = end_at

                request = alpaca_data_requests.StockBarsRequest(
                    symbol_or_symbols=tickers_chunk,
                    start=start_at_chunk,
                    end=end_at_chunk,
                    timeframe=timeframe.TimeFrame.Day,
                    adjustment='all',
                )

                response = self.alpaca_historical_client.get_stock_bars(
                    request,
                )

                for ticker in response:
                    ticker_bars = [{
                        'timestamp': datetime.datetime.strptime(
                            row['t'],
                            '%Y-%m-%dT%H:%M:%SZ',
                        ).replace(
                            tzinfo=None,
                            hour=0,
                            minute=0,
                            second=0,
                        ),
                        'ticker': ticker,
                        'open_price': round(float(row['o']), 2),
                        'high_price': round(float(row['h']), 2),
                        'low_price': round(float(row['l']), 2),
                        'close_price': round(float(row['c']), 2),
                        'volume': round(float(row['v']), 2),
                        'source': SOURCE_ALPACA,
                    } for row in response[ticker]]

                    bars.extend(ticker_bars)

        dataframe = pandas.DataFrame.from_dict(
            data=bars,
        )

        if self.print_logs:
            runtime_stop = datetime.datetime.now()

            runtime_in_minutes = (
                runtime_stop - self.runtime_start,
            ).total_seconds() / 60

            print('ending get range data')
            print('runtime {} minutes'.format(runtime_in_minutes))

        return dataframe
