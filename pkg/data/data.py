import datetime
import time

import requests
import pandas
from alpaca.data import historical
from alpaca.data import requests as alpaca_data_requests
from alpaca.data import timeframe
import bs4


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
        edgar_user_agent: str,
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
        self.edgar_user_agent = edgar_user_agent
        self.edgar_requests_per_second = 10  # EDGAR rate limitation
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
            print("beginning get range equities data")

        start_at = start_at.replace(hour=0, minute=0, second=0)
        end_at = end_at.replace(hour=0, minute=0, second=0)

        difference_in_days = (end_at - start_at).days

        bars: list[dict[str, any]] = []
        # chunking requests due to Alpaca request limitations
        for i in range(0, len(tickers), self.alpaca_ticker_chunk_size):
            tickers_chunk = tickers[i:i+self.alpaca_ticker_chunk_size]

            if self.print_logs:
                print("getting {} bars".format(tickers_chunk))

            for j in range(0, difference_in_days, self.alpaca_datetime_chunk_size_in_days):
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
                    ticker_bars = [{
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
                    } for row in response[ticker]]

                    bars.extend(ticker_bars)

        all_bars = pandas.DataFrame.from_dict(
            data=bars,
        )

        if self.print_logs:
            runtime_stop = datetime.datetime.now()

            runtime_in_minutes = (
                runtime_stop - self.runtime_start
            ).total_seconds() / 60

            print("ending get range equities data")
            print("runtime {} minutes".format(round(runtime_in_minutes, 2)))

        return all_bars

    def get_range_corporate_filings(
        self,
        tickers: list[str],
        start_at: datetime.datetime,
        end_at: datetime.datetime,
    ) -> list[dict[str, any]]:
        if self.print_logs:
            self.runtime_start = datetime.datetime.now()
            print("beginning get range corporate filings data")

        response = self.http_client.get(
            url="https://www.sec.gov/files/company_tickers.json",
            headers={
                "User-Agent": self.edgar_user_agent,
                "Accept-Encoding": "gzip, deflate",
                "Host": "www.sec.gov",
            }
        )

        time.sleep(1 / self.edgar_requests_per_second)

        ciks_response_json = response.json()

        ciks_and_tickers = []

        for key in ciks_response_json.keys():
            row = ciks_response_json[key]
            if row["ticker"] in tickers:
                ciks_and_tickers.append({
                    "ticker": row["ticker"],
                    "cik": row["cik_str"],
                })

        corporate_filings = []

        for index in range(len(ciks_and_tickers)):
            cik = ciks_and_tickers[index]["cik"]
            ticker = ciks_and_tickers[index]["ticker"]

            if self.print_logs:
                print("getting {} corporate filings".format(ticker))

            submission_response = self.http_client.get(
                url="https://data.sec.gov/submissions/CIK{:0>10}.json".format(
                    cik
                ),
                headers={
                    "User-Agent": self.edgar_user_agent,
                    "Accept-Encoding": "gzip, deflate",
                    "Host": "data.sec.gov",
                }
            )

            time.sleep(1 / self.edgar_requests_per_second)

            submission_response_json = submission_response.json()

            recent_filings = submission_response_json["filings"]["recent"]

            target_forms = ["10-K", "10-Q", "8-K"]

            ticker_corporate_filings = {}

            for target_form in target_forms:
                forms_information = self._get_forms_information(
                    start_at=start_at,
                    end_at=end_at,
                    accession_numbers=recent_filings["accessionNumber"],
                    acceptance_dates=recent_filings["acceptanceDateTime"],
                    forms=recent_filings["form"],
                    target_form=target_form,
                )

                form_contents = self._get_forms_contents(
                    cik=cik,
                    forms_information=forms_information,
                )

                ticker_corporate_filings[target_form] = form_contents

            corporate_filings.append({
                "ticker": ticker,
                "corporate_filings": ticker_corporate_filings,
            })

        if self.print_logs:
            runtime_stop = datetime.datetime.now()

            runtime_in_minutes = (
                runtime_stop - self.runtime_start
            ).total_seconds() / 60

            print("ending get range corporate filings data")
            print("runtime {} minutes".format(round(runtime_in_minutes, 2)))

        return corporate_filings

    def _get_forms_information(
        self,
        start_at: datetime.datetime,
        end_at: datetime.datetime,
        accession_numbers: list[str],
        acceptance_dates: list[str],
        forms: list[str],
        target_form: str,
    ) -> list[dict[str, any]]:
        indices = [
            index for index, form in enumerate(forms)
            if form == target_form
        ]

        forms_information = []
        for index in indices:
            acceptance_date = datetime.datetime.fromisoformat(
                acceptance_dates[index][:-1], # remove trailing "Z"
            )

            if acceptance_date >= start_at and acceptance_date <= end_at:
                forms_information.append({
                    "accession_number": accession_numbers[index],
                    "acceptance_date": acceptance_date,
                })

        return forms_information

    def _get_forms_contents(
        self,
        cik: str,
        forms_information: list[dict[str, any]],
    ) -> list[dict[str, any]]:
        forms_contents = []

        for form_information in forms_information:
            response = self.http_client.get(
                url="https://www.sec.gov/Archives/edgar/data/{}/{}.txt".format(
                    cik,
                    form_information["accession_number"],
                ),
                headers={
                    "User-Agent": self.edgar_user_agent,
                    "Accept-Encoding": "gzip, deflate",
                    "Host": "www.sec.gov",
                }
            )

            parser = bs4.BeautifulSoup(response.text, "xml")

            forms_contents.append({
                "acceptance_date": form_information["acceptance_date"],
                "content": list(parser.stripped_strings),
            })

            time.sleep(1 / self.edgar_requests_per_second)

        return forms_contents
