import unittest
import datetime

from alpaca.data import requests as alpaca_data_requests

from pkg.data import data


class MockAlpacaHistoricalResponse:
    def __init__(
        self,
        data: dict[str, any],
    ) -> any:
        self.data = data
        self.keys = list(self.data.keys())
        self.index = 0

    def __iter__(self) -> any:
        return self

    def __next__(self) -> any:
        if self.index < len(self.keys):
            key = self.keys[self.index]
            self.index += 1
            return key
        else:
            raise StopIteration

    def __getitem__(
        self,
        key: str,
    ) -> any:
        return self.data[key]


class MockAlpacaHistoricalClient:
    def __init__(
        self,
        response: MockAlpacaHistoricalResponse,
        exception: Exception,
    ) -> None:
        self.response = response
        self.exception = exception

    def get_stock_bars(
        self,
        request: alpaca_data_requests.StockBarsRequest,
    ) -> any:
        if self.exception is not None:
            raise self.exception

        return self.response


class MockHTTPGetResponse:
    def __init__(
        self,
        text: str = None,
        data: dict[str, any] = None,
    ) -> None:
        self.text = text
        self.data = data

    def json(self) -> any:
        return self.data


class MockHttpClient:
    def __init__(
        self,
        responses: dict[str, any],
        exceptions: dict[str, Exception],
    ) -> None:
        self.responses = responses
        self.exceptions = exceptions

    def get(
        self,
        url: str,
        headers: dict[str, str],
    ) -> any:
        if self.exceptions is not None:
            keys = list(self.exceptions.keys())
            for key in keys:
                if key in url:
                    raise self.exceptions[key]

        if self.responses is not None:
            keys = list(self.responses.keys())
            for key in keys:
                if key in url:
                    return self.responses[key]


def mock_get_forms_information_error(
    start_at: datetime.datetime,
    end_at: datetime.datetime,
    accession_numbers: list[str],
    acceptance_dates: list[str],
    forms: list[str],
    target_form: str,
) -> list[dict[str, any]]:
    _ = start_at, end_at, accession_numbers, acceptance_dates, forms, target_form

    raise Exception("get forms information error")


def mock_get_forms_information_success(
    start_at: datetime.datetime,
    end_at: datetime.datetime,
    accession_numbers: list[str],
    acceptance_dates: list[str],
    forms: list[str],
    target_form: str,
) -> list[dict[str, any]]:
    _ = start_at, end_at, accession_numbers, acceptance_dates, forms, target_form

    return [
        {
            "accession_number": "0001171843-24-001239",
            "acceptance_date": "1977-05-26T18:36:45.000Z",
        },
    ]


def mock_get_forms_contents_error(
    cik: str,
    forms_information: list[dict[str, any]],
) -> list[dict[str, any]]:
    _ = cik, forms_information

    raise Exception("get forms contents error")


def mock_get_forms_contents_success(
    cik: str,
    forms_information: list[dict[str, any]],
) -> list[dict[str, any]]:
    _ = cik, forms_information

    return [
        {
            "acceptance_date": "1977-05-26T18:36:45.000Z",
            "content": "form contents",
        },
    ]


class TestGetRangeEquitiesBars(unittest.TestCase):
    def test_get_range_equities_bars_alpaca_get_stock_bars_error(self) -> None:
        client = data.Client(
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            edgar_user_agent="edgar_user_agent",
        )

        client.alpaca_historical_client = MockAlpacaHistoricalClient(
            response=None,
            exception=Exception("get stock bars error"),
        )

        with self.assertRaises(Exception) as context:
            _ = client.get_range_equities_bars(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
            )

        self.assertEqual("get stock bars error", str(context.exception))

    def test_get_range_equities_bars_success(self) -> None:
        client = data.Client(
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            edgar_user_agent="edgar_user_agent",
        )

        client.alpaca_historical_client = MockAlpacaHistoricalClient(
            response=MockAlpacaHistoricalResponse(
                data={
                    "TICKER": [
                        {
                            "t": "1977-05-25T00:00:00Z",
                            "o": "5.0",
                            "h": "6.0",
                            "l": "4.0",
                            "c": "5.0",
                            "v": "100.0",
                        },
                        {
                            "t": "1977-05-25T00:00:00Z",
                            "o": "6.0",
                            "h": "7.0",
                            "l": "5.0",
                            "c": "6.0",
                            "v": "200.0",
                        },
                        {
                            "t": "1977-05-25T00:00:00Z",
                            "o": "7.0",
                            "h": "8.0",
                            "l": "6.0",
                            "c": "7.0",
                            "v": "300.0",
                        },
                    ],
                },
            ),
            exception=None,
        )

        range_equities_bars = client.get_range_equities_bars(
            tickers=["TICKER"],
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
        )

        self.assertEqual(3, len(range_equities_bars))
        self.assertEqual("TICKER", range_equities_bars.iloc[0]["ticker"])
        self.assertEqual(5.0, range_equities_bars.iloc[0]["open_price"])
        self.assertEqual(6.0, range_equities_bars.iloc[0]["high_price"])
        self.assertEqual(4.0, range_equities_bars.iloc[0]["low_price"])
        self.assertEqual(5.0, range_equities_bars.iloc[0]["close_price"])
        self.assertEqual(100.0, range_equities_bars.iloc[0]["volume"])
        self.assertEqual("ALPACA", range_equities_bars.iloc[0]["source"])


class TestPrivateGetFormsInformation(unittest.TestCase):
    def test_private_get_forms_information_success(self) -> None:
        client = data.Client(
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            edgar_user_agent="edgar_user_agent",
        )

        forms_information = client._get_forms_information(
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
            accession_numbers=[
                "0001171843-24-001239",
                "0001193125-15-118890",
            ],
            acceptance_dates=[
                "1977-05-26T18:36:45.000Z",
                "1977-05-28T18:36:45.000Z",
            ],
            forms=[
                "10-K",
                "10-K",
            ],
            target_form="10-K",
        )

        self.assertEqual(
            forms_information[0]["accession_number"],
            "0001171843-24-001239",
        )
        self.assertEqual(
            forms_information[0]["acceptance_date"],
            datetime.datetime(
                1977,
                5,
                26,
                18,
                36,
                45,
            ),
        )


class TestPrivateGetFormsContents(unittest.TestCase):
    def test_private_get_forms_contents_success(self) -> None:
        client = data.Client(
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            edgar_user_agent="edgar_user_agent",
        )

        client.http_client = MockHttpClient(
            responses={
                "edgar/data": MockHTTPGetResponse(
                    # text=['form contents']
                    text="<xml>form contents</xml>",
                ),
            },
            exceptions=None,
        )

        forms_contents = client._get_forms_contents(
            cik="0001123494",
            forms_information=[
                {
                    "acceptance_date": datetime.datetime(
                        1977,
                        5,
                        26,
                        18,
                        36,
                        45,
                        tzinfo=datetime.timezone.utc,
                    ),
                    "accession_number": "0001171843-24-001239",
                },
            ],
        )

        self.assertEqual(
            forms_contents[0]["acceptance_date"],
            datetime.datetime(
                1977,
                5,
                26,
                18,
                36,
                45,
                tzinfo=datetime.timezone.utc,
            ),
        )

        self.assertEqual(
            forms_contents[0]["content"],
            ["form contents"],
        )


class TestGetRangeCorporateFilings(unittest.TestCase):
    def setUp(self) -> None:
        self.client = data.Client(
            alpaca_api_key="alpaca_api_key",
            alpaca_api_secret="alpaca_api_secret",
            edgar_user_agent="edgar_user_agent",
        )

    def tearDown(self) -> None:
        pass

    def test_get_range_corporate_filings_get_tickers_http_error(self) -> None:
        self.client.http_client = MockHttpClient(
            responses={},
            exceptions={
                "www.sec.gov": Exception("get tickers http error"),
            },
        )

        with self.assertRaises(Exception) as context:
            _ = self.client.get_range_corporate_filings(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
            )

        self.assertEqual("get tickers http error", str(context.exception))

    def test_get_range_corporate_filings_get_submissions_http_error(self) -> None:
        self.client.http_client = MockHttpClient(
            responses={
                "sec.gov": MockHTTPGetResponse(
                    data={
                        "0": {
                            "cik_str": 1123494,
                            "ticker": "TICKER",
                        },
                    },
                ),
            },
            exceptions={
                "data.sec.gov": Exception("get submissions http error"),
            },
        )

        with self.assertRaises(Exception) as context:
            _ = self.client.get_range_corporate_filings(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
            )

        self.assertEqual("get submissions http error", str(context.exception))

    def test_get_range_corporate_filings_get_forms_information_error(self) -> None:
        self.client.http_client = MockHttpClient(
            responses={
                "www.sec.gov": MockHTTPGetResponse(
                    data={
                        "0": {
                            "cik_str": 1123494,
                            "ticker": "TICKER",
                        },
                    },
                ),
                "data.sec.gov": MockHTTPGetResponse(
                    data={
                        "filings": {
                            "recent": {
                                "accessionNumber": ["0001171843-24-001239"],
                                "acceptanceDateTime": ["1977-05-26T18:36:45.000Z"],
                                "form": ["10-K"],
                            },
                        },
                    },
                ),
            },
            exceptions=None,
        )

        self.client._get_forms_information = mock_get_forms_information_error

        with self.assertRaises(Exception) as context:
            _ = self.client.get_range_corporate_filings(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
            )

        self.assertEqual("get forms information error", str(context.exception))

    def test_get_range_corporate_filings_get_forms_contents_error(self) -> None:
        self.client.http_client = MockHttpClient(
            responses={
                "www.sec.gov/files/company": MockHTTPGetResponse(
                    data={
                        "0": {
                            "cik_str": 1123494,
                            "ticker": "TICKER",
                        },
                    },
                ),
                "data.sec.gov/submissions": MockHTTPGetResponse(
                    data={
                        "filings": {
                            "recent": {
                                "accessionNumber": ["0001171843-24-001239"],
                                "acceptanceDateTime": ["1977-05-26T18:36:45.000Z"],
                                "form": ["10-K"],
                            },
                        },
                    },
                ),
            },
            exceptions=None,
        )

        self.client._get_forms_information = mock_get_forms_information_success
        self.client._get_forms_contents = mock_get_forms_contents_error

        with self.assertRaises(Exception) as context:
            _ = self.client.get_range_corporate_filings(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
            )

        self.assertEqual("get forms contents error", str(context.exception))

    def test_get_range_corporate_filings_success(self) -> None:
        self.client.http_client = MockHttpClient(
            responses={
                "www.sec.gov/files/company": MockHTTPGetResponse(
                    data={
                        "0": {
                            "cik_str": 1123494,
                            "ticker": "TICKER",
                        },
                    },
                ),
                "data.sec.gov/submissions": MockHTTPGetResponse(
                    data={
                        "filings": {
                            "recent": {
                                "accessionNumber": ["0001171843-24-001239"],
                                "acceptanceDateTime": ["1977-05-26T18:36:45.000Z"],
                                "form": ["10-K"],
                            },
                        },
                    },
                ),
            },
            exceptions=None,
        )

        self.client._get_forms_information = mock_get_forms_information_success
        self.client._get_forms_contents = mock_get_forms_contents_success

        corporate_filings = self.client.get_range_corporate_filings(
            tickers=["TICKER"],
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d"),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d"),
        )

        corporate_filing = corporate_filings[0]

        self.assertEqual(corporate_filing["ticker"], "TICKER")

        self.assertEqual(len(corporate_filing["corporate_filings"].keys()), 3)
