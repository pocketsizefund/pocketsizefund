import datetime

import requests
import pandas
from alpaca.data import historical
from alpaca.data import requests as alpaca_data_requests
from alpaca.data import timeframe


ALPACA_TICKER_CHUNK_SIZE = 50
ALPACA_DATETIME_CHUNK_SIZE_IN_DAYS = 200

SOURCE_ALPACA = "ALPACA"

COLUMN_TIMESTAMP = "timestamp"
COLUMN_TICKER = "ticker"
COLUMN_OPEN_PRICE = "open_price"
COLUMN_HIGH_PRICE = "high_price"
COLUMN_LOW_PRICE = "low_price"
COLUMN_CLOSE_PRICE = "close_price"
COLUMN_VOLUME = "volume"
COLUMN_SOURCE = "source"


class Client:
    def __init__(
        self,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        print_logs: bool = False,
    ) -> None:
        self.alpaca_ticker_chunk_size = ALPACA_TICKER_CHUNK_SIZE
        self.alpaca_datetime_chunk_size_in_days = ALPACA_DATETIME_CHUNK_SIZE_IN_DAYS
        self.alpaca_historical_client = historical.StockHistoricalDataClient(
            api_key=alpaca_api_key,
            secret_key=alpaca_api_secret,
            raw_data=True,
        )
        self.http_client = requests
        self.print_logs = print_logs
        self.runtime_start = None

    def get_range_equities_bars(
        self,
        tickers: list[str],
        start_at: datetime.datetime,
        end_at: datetime.datetime,
    ) -> pandas.DataFrame:
        if self.print_logs:
            self.runtime_start = datetime.datetime.now()
            print("beginning get range data")

        start_at = start_at.replace(hour=0, minute=0, second=0)
        end_at = end_at.replace(hour=0, minute=0, second=0)

        difference_in_days = (end_at - start_at).days

        bars: list[dict[str, any]] = []
        # chunking requests due to Alpaca request limitations
        for i in range(0, len(tickers), self.alpaca_ticker_chunk_size):
            tickers_chunk = tickers[i : i + self.alpaca_ticker_chunk_size]

            if self.print_logs:
                print("getting {} bars".format(tickers_chunk))

            for j in range(
                0, difference_in_days, self.alpaca_datetime_chunk_size_in_days
            ):
                start_at_chunk = start_at + datetime.timedelta(days=j)
                end_at_chunk = start_at_chunk + datetime.timedelta(
                    days=self.alpaca_datetime_chunk_size_in_days,
                )

                if end_at_chunk > end_at:
                    end_at_chunk = end_at

                request = alpaca_data_requests.StockBarsRequest(
                    symbol_or_symbols=tickers_chunk,
                    start=start_at_chunk,
                    end=end_at_chunk,
                    timeframe=timeframe.TimeFrame.Day,
                    adjustment="all",
                )

                response = self.alpaca_historical_client.get_stock_bars(
                    request,
                )

                for ticker in response:
                    ticker_bars = [
                        {
                            COLUMN_TIMESTAMP: datetime.datetime.strptime(
                                row["t"],
                                "%Y-%m-%dT%H:%M:%SZ",
                            ).replace(
                                tzinfo=None,
                                hour=0,
                                minute=0,
                                second=0,
                            ),
                            COLUMN_TICKER: ticker,
                            COLUMN_OPEN_PRICE: round(float(row["o"]), 2),
                            COLUMN_HIGH_PRICE: round(float(row["h"]), 2),
                            COLUMN_LOW_PRICE: round(float(row["l"]), 2),
                            COLUMN_CLOSE_PRICE: round(float(row["c"]), 2),
                            COLUMN_VOLUME: round(float(row["v"]), 2),
                            COLUMN_SOURCE: SOURCE_ALPACA,
                        }
                        for row in response[ticker]
                    ]

                    bars.extend(ticker_bars)

        all_bars = pandas.DataFrame.from_dict(
            data=bars,
        )

        if self.print_logs:
            runtime_stop = datetime.datetime.now()

            runtime_in_minutes = (
                runtime_stop - self.runtime_start
            ).total_seconds() / 60

            print("ending get range data")
            print("runtime {} minutes".format(round(runtime_in_minutes, 2)))

        return all_bars
