import datetime
from typing import Optional

import pytest
from alpaca.data import requests as alpaca_data_requests

from pkg.config import config
from pkg.data import data

ALPACA_API_KEY = "alpaca_api_key"  # noqa: S105
ALPACA_API_SECRET = "alpaca_api_secret"  # noqa: S105
EDGAR_USER_AGENT = "edgar_user_agent"  # noqa: S105


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
        request: alpaca_data_requests.StockBarsRequest,  # noqa: ARG002
    ) -> any:
        """Get stock bars."""
        if self.exception is not None:
            raise self.exception

        return self.response


class MockHTTPGetResponse:
    def __init__(
        self,
        text: Optional[str] = None,  # noqa: UP007
        data: Optional[dict[str, any]] = None,  # noqa: UP007
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
        headers: dict[str, str],  # noqa: ARG002
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


def mock_get_forms_information_error(  # noqa: PLR0913
    start_at: datetime.datetime,
    end_at: datetime.datetime,
    accession_numbers: list[str],
    acceptance_dates: list[str],
    forms: list[str],
    target_form: str,
) -> list[dict[str, any]]:
    _ = start_at, end_at, accession_numbers, acceptance_dates, forms, target_form

    msg = "get forms information error"
    raise ValueError(msg)


def mock_get_forms_information_success(  # noqa: PLR0913
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

    msg = "get forms contents error"
    raise ValueError(msg)


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


def test_get_range_equities_bars_alpaca_get_stock_bars_error() -> None:
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

    with pytest.raises(Exception, match="get stock bars"):
        _ = client.get_range_equities_bars(
            tickers=["TICKER"],
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
        )


def test_get_range_equities_bars_success() -> None:
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
            tzinfo=config.TIMEZONE,
        ),
        end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
            tzinfo=config.TIMEZONE,
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


def test_private_get_forms_information_success() -> None:
    client = data.Client(
        alpaca_api_key=ALPACA_API_KEY,
        alpaca_api_secret=ALPACA_API_SECRET,
        edgar_user_agent=EDGAR_USER_AGENT,
    )

    forms_information = client._get_forms_information(
        start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
            tzinfo=config.TIMEZONE,
        ),
        end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
            tzinfo=config.TIMEZONE,
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
        1977,
        5,
        26,
        18,
        36,
        45,
        tzinfo=config.TIMEZONE,
    )


def test_private_get_forms_contents_success() -> None:
    client = data.Client(
        alpaca_api_key=ALPACA_API_KEY,
        alpaca_api_secret=ALPACA_API_SECRET,
        edgar_user_agent=EDGAR_USER_AGENT,
    )

    client.http_client = MockHttpClient(
        responses={
            "edgar/data": MockHTTPGetResponse(
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
                    tzinfo=config.TIMEZONE,
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
        tzinfo=config.TIMEZONE,
    )

    assert forms_contents[0]["content"] == ["form contents"]


def test_get_range_corporate_filings_get_tickers_http_error() -> None:
    client = data.Client(
        alpaca_api_key=ALPACA_API_KEY,
        alpaca_api_secret=ALPACA_API_SECRET,
        edgar_user_agent=EDGAR_USER_AGENT,
    )

    client.http_client = MockHttpClient(
        responses={},
        exceptions={
            "www.sec.gov": Exception("get tickers http error"),
        },
    )

    with pytest.raises(Exception, match="get tickers http"):
        _ = client.get_range_corporate_filings(
            tickers=["TICKER"],
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
        )


def test_get_range_corporate_filings_get_submissions_http_error() -> None:
    client = data.Client(
        alpaca_api_key=ALPACA_API_KEY,
        alpaca_api_secret=ALPACA_API_SECRET,
        edgar_user_agent=EDGAR_USER_AGENT,
    )

    client.http_client = MockHttpClient(
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

    with pytest.raises(Exception, match="get submissions http"):
        _ = client.get_range_corporate_filings(
            tickers=["TICKER"],
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
        )


def test_get_range_corporate_filings_get_forms_information_error() -> None:
    client = data.Client(
        alpaca_api_key=ALPACA_API_KEY,
        alpaca_api_secret=ALPACA_API_SECRET,
        edgar_user_agent=EDGAR_USER_AGENT,
    )

    client.http_client = MockHttpClient(
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

    client._get_forms_information = mock_get_forms_information_error

    with pytest.raises(Exception, match="get forms information"):
        _ = client.get_range_corporate_filings(
            tickers=["TICKER"],
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
        )


def test_get_range_corporate_filings_get_forms_contents_error() -> None:
    client = data.Client(
        alpaca_api_key=ALPACA_API_KEY,
        alpaca_api_secret=ALPACA_API_SECRET,
        edgar_user_agent=EDGAR_USER_AGENT,
    )

    client.http_client = MockHttpClient(
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

    client._get_forms_information = mock_get_forms_information_success
    client._get_forms_contents = mock_get_forms_contents_error

    with pytest.raises(Exception, match="get forms contents"):
        _ = client.get_range_corporate_filings(
            tickers=["TICKER"],
            start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
            end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
                tzinfo=config.TIMEZONE,
            ),
        )


def test_get_range_corporate_filings_success() -> None:
    client = data.Client(
        alpaca_api_key=ALPACA_API_KEY,
        alpaca_api_secret=ALPACA_API_SECRET,
        edgar_user_agent=EDGAR_USER_AGENT,
    )

    client.http_client = MockHttpClient(
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

    client._get_forms_information = mock_get_forms_information_success
    client._get_forms_contents = mock_get_forms_contents_success

    corporate_filings = client.get_range_corporate_filings(
        tickers=["TICKER"],
        start_at=datetime.datetime.strptime("1977-05-25", "%Y-%m-%d").replace(
            tzinfo=config.TIMEZONE,
        ),
        end_at=datetime.datetime.strptime("1977-05-28", "%Y-%m-%d").replace(
            tzinfo=config.TIMEZONE,
        ),
    )

    corporate_filing = corporate_filings[0]

    assert corporate_filing["ticker"] == "TICKER"
    assert len(corporate_filing["corporate_filings"].keys()) == 3
