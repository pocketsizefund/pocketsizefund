from library.trade import Client
from unittest.mock import patch


class MockResponse:
    def __init__(
        self,
        status_code: int,
        json_data: dict,
    ):
        self.status_code = status_code
        self.json_data = json_data

    def status_code(self):
        return self.status_code

    def json(self):
        return self.json_data


@patch("requests.get")
def test_get_account(mock_get) -> None:
    mock_response = mock_get.return_value
    mock_response.status_code = 200
    mock_response.json.return_value = {
        "cash": "123.0",
        "long_market_value": "456.0",
        "short_market_value": "789.0",
    }

    client = Client(
        alpaca_api_key="test_api_key",
        alpaca_api_secret="test_api_secret",
        is_paper=True,
    )

    result = client.get_account()

    assert result["cash_amount"] == 123.0
    assert result["long_market_value"] == 456.0
    assert result["short_market_value"] == 789.0
