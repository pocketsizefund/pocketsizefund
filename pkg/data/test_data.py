"""Unit tests for Alpaca Client."""

import datetime
import unittest

import pytest
from alpaca.data import requests as alpaca_data_requests

from pkg.config import config
from pkg.data import data

ALPACA_API_KEY = "alpaca_api_key"  # noqa: S106
ALPACA_API_SECRET = "alpaca_api_secret"  # noqa: S106
EDGAR_USER_AGENT = "edgar_user_agent"  # noqa: S106


class MockAlpacaHistoricalResponse:
    """Mock Alpaca historical response."""

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
        raise StopIteration

    def __getitem__(
        self,
        key: str,
    ) -> any:
        return self.data[key]


class MockAlpacaHistoricalClient:
    """Mock Alpaca historical client."""

    def __init__(
        self,
        response: MockAlpacaHistoricalResponse,
        exception: Exception,
    ) -> None:
        self.response = response
        self.exception = exception

    def get_stock_bars(
        self,
        request: alpaca_data_requests.StockBarsRequest, # noqa: ARG002
    ) -> any:
        """Get stock bars."""
        if self.exception is not None:
            raise self.exception

        return self.response


class MockHTTPGetResponse:
    """Mock HTTP response."""

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
    """Mock HTTP client."""

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
        headers: dict[str, str], # noqa: ARG002
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

        return None


def mock_get_forms_information_error(
    start_at: datetime.datetime,
    end_at: datetime.datetime,
    accession_numbers: list[str],
    acceptance_dates: list[str],
    forms: list[str],
    target_form: str,
) -> list[dict[str, any]]:
    """Mock get forms information error."""
    _ = start_at, end_at, accession_numbers, acceptance_dates, forms, target_form

    msg = "get forms information error"
    raise ValueError(msg)


def mock_get_forms_information_success(
    start_at: datetime.datetime,
    end_at: datetime.datetime,
    accession_numbers: list[str],
    acceptance_dates: list[str],
    forms: list[str],
    target_form: str,
) -> list[dict[str, any]]:
    """Mock get forms information success."""
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
    """Mock get forms contents error."""
    _ = cik, forms_information

    msg = "get forms contents error"
    raise ValueError(msg)


def mock_get_forms_contents_success(
    cik: str,
    forms_information: list[dict[str, any]],
) -> list[dict[str, any]]:
    """Mock get forms contents success."""
    _ = cik, forms_information

    return [
        {
            "acceptance_date": "1977-05-26T18:36:45.000Z",
            "content": "form contents",
        },
    ]


class TestGetRangeEquitiesBars(unittest.TestCase):
    """Unit tests for get range equities bars."""

    def test_get_range_equities_bars_alpaca_get_stock_bars_error(self) -> None:
        """Test get range equities bars alpaca get stock bars error."""
        client = data.Client(
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            edgar_user_agent=EDGAR_USER_AGENT,
        )

        client.alpaca_historical_client = MockAlpacaHistoricalClient(
            response=None,
            exception=Exception("get stock bars error"),
        )

        with pytest.raises(Exception) as context:
            _ = client.get_range_equities_bars(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
            )

        assert str(context.exception) == "get stock bars error"

    def test_get_range_equities_bars_success(self) -> None:
        """Test get range equities bars success."""
        client = data.Client(
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            edgar_user_agent=EDGAR_USER_AGENT,
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
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE
            ),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE
            ),
        )

        assert len(range_equities_bars) == 3
        assert range_equities_bars.iloc[0]["ticker"] == "TICKER"
        assert range_equities_bars.iloc[0]["open_price"] == 5.0
        assert range_equities_bars.iloc[0]["high_price"] == 6.0
        assert range_equities_bars.iloc[0]["low_price"] == 4.0
        assert range_equities_bars.iloc[0]["close_price"] == 5.0
        assert range_equities_bars.iloc[0]["volume"] == 100.0
        assert range_equities_bars.iloc[0]["source"] == "ALPACA"


class TestPrivateGetFormsInformation(unittest.TestCase):
    """Unit tests for private get forms information."""

    def test_private_get_forms_information_success(self) -> None:
        """Test private get forms information success."""
        client = data.Client(
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            edgar_user_agent=EDGAR_USER_AGENT,
        )

        forms_information = client._get_forms_information(
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE
            ),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE
            ),
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

        assert forms_information[0]["accession_number"] == "0001171843-24-001239"
        assert forms_information[0]["acceptance_date"] == datetime.datetime(
            1977, 5, 26, 18, 36, 45, tzinfo=config.TIMEZONE
        )


class TestPrivateGetFormsContents(unittest.TestCase):
    """Unit tests for private get forms contents."""

    def test_private_get_forms_contents_success(self) -> None:
        """Test private get forms contents success."""
        client = data.Client(
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            edgar_user_agent=EDGAR_USER_AGENT,
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

        assert forms_contents[0]["acceptance_date"] == datetime.datetime(
            1977,
            5,
            26,
            18,
            36,
            45,
            tzinfo=datetime.timezone.utc,
        )

        forms_contents[0]["content"] == ["form contents"]


class TestGetRangeCorporateFilings(unittest.TestCase):
    """Unit tests for get range corporate filings."""

    def setUp(self) -> None:
        """Set up test."""
        self.client = data.Client(
            alpaca_api_key=ALPACA_API_KEY,
            alpaca_api_secret=ALPACA_API_SECRET,
            edgar_user_agent=EDGAR_USER_AGENT,
        )

    def tearDown(self) -> None:
        """Tear down test."""
        pass

    def test_get_range_corporate_filings_get_tickers_http_error(self) -> None:
        """Test get range corporate filings get tickers http error."""
        self.client.http_client = MockHttpClient(
            responses={},
            exceptions={
                "www.sec.gov": Exception("get tickers http error"),
            },
        )

        with pytest.raises(Exception) as context:
            _ = self.client.get_range_corporate_filings(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
            )

        assert str(context.exception) == "get tickers http error"

    def test_get_range_corporate_filings_get_submissions_http_error(self) -> None:
        """Test get range corporate filings get submissions http error."""
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

        with pytest.raises(Exception) as context:
            _ = self.client.get_range_corporate_filings(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
            )

        assert str(context.exception) == "get submissions http error"

    def test_get_range_corporate_filings_get_forms_information_error(self) -> None:
        """Test get range corporate filings get forms information error."""
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

        with pytest.raises(Exception) as context:
            _ = self.client.get_range_corporate_filings(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
            )

        assert str(context.exception) == "get forms information error"

    def test_get_range_corporate_filings_get_forms_contents_error(self) -> None:
        """Test get range corporate filings get forms contents error."""
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

        with pytest.raises(Exception) as context:
            _ = self.client.get_range_corporate_filings(
                tickers=["TICKER"],
                start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
                end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                    tzinfo=config.TIMEZONE
                ),
            )

        assert str(context.exception) == "get forms contents error"

    def test_get_range_corporate_filings_success(self) -> None:
        """Test get range corporate filings success."""
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
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE
            ),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE
            ),
        )

        corporate_filing = corporate_filings[0]

        assert corporate_filing["ticker"] == "TICKER"
        assert len(corporate_filing["corporate_filings"].keys()) == 3
