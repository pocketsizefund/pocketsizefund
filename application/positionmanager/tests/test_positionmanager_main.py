import unittest
from decimal import Decimal
from unittest.mock import MagicMock, patch

from fastapi import HTTPException, status
from fastapi.testclient import TestClient

from application.positionmanager.src.positionmanager.clients import AlpacaClient
from application.positionmanager.src.positionmanager.main import application
from application.positionmanager.src.positionmanager.models import Money

client = TestClient(application)


def test_health_check() -> None:
    response = client.get("/health")
    assert response.status_code == status.HTTP_200_OK


class TestCreatePositionsEndpoint(unittest.TestCase):
    @patch("application.positionmanager.src.positionmanager.main.AlpacaClient")
    @patch("application.positionmanager.src.positionmanager.main.DataClient")
    @patch("application.positionmanager.src.positionmanager.main.requests.get")
    def test_open_position_success(
        self,
        mock_requests_get: MagicMock,
        MockDataClient: MagicMock,  # noqa: N803
        MockAlpacaClient: MagicMock,  # noqa: N803
    ) -> None:
        mock_alpaca_instance = MagicMock(spec=AlpacaClient)
        mock_cash_balance = Money(amount=Decimal("100000.00"))
        mock_alpaca_instance.get_cash_balance.return_value = mock_cash_balance
        mock_alpaca_instance.place_notional_order.return_value = None
        MockAlpacaClient.return_value = mock_alpaca_instance

        mock_data_client_instance = MagicMock()
        mock_historical_data = MagicMock()
        mock_data_client_instance.get_data.return_value = mock_historical_data
        MockDataClient.return_value = mock_data_client_instance

        mock_response = MagicMock()
        mock_response.content = b"mock_arrow_data"
        mock_response.raise_for_status.return_value = None
        mock_requests_get.return_value = mock_response

        with (
            patch(
                "application.positionmanager.src.positionmanager.clients.pa.py_buffer"
            ),
            patch(
                "application.positionmanager.src.positionmanager.clients.pa.ipc.RecordBatchStreamReader"
            ),
            patch(
                "application.positionmanager.src.positionmanager.main.pl.DataFrame"
            ) as mock_pl_dataframe,
            patch(
                "application.positionmanager.src.positionmanager.main.PortfolioOptimizer"
            ) as MockPortfolioOptimizer,  # noqa: N806
        ):
            mock_df = MagicMock()
            mock_df.unique.return_value.to_list.return_value = ["AAPL", "MSFT"]
            mock_pl_dataframe.return_value = mock_df

            mock_optimizer_instance = MagicMock()
            mock_optimizer_instance.get_optimized_portfolio.return_value = {
                "AAPL": 50.0,
                "MSFT": 30.0,
            }
            MockPortfolioOptimizer.return_value = mock_optimizer_instance

            cloud_event_data = {
                "source": "test",
                "type": "test.event",
                "data": {
                    "predictions": {
                        "AAPL": {"percentile_50": [0.1, 0.2, 0.3]},
                        "MSFT": {"percentile_50": [0.2, 0.3, 0.4]},
                    }
                },
            }
            response = client.post("/positions/open", json=cloud_event_data)

        assert response.status_code == status.HTTP_200_OK
        response_data = response.json()
        assert "source" in response_data
        assert "type" in response_data

    @patch("application.positionmanager.src.positionmanager.main.AlpacaClient")
    def test_open_position_alpaca_error(self, MockAlpacaClient: MagicMock) -> None:  # noqa: N803
        mock_alpaca_instance = MagicMock(spec=AlpacaClient)
        mock_alpaca_instance.get_cash_balance.side_effect = HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Error getting cash balance",
        )
        MockAlpacaClient.return_value = mock_alpaca_instance

        cloud_event_data = {
            "source": "test",
            "type": "test.event",
            "data": {"predictions": {"AAPL": {"percentile_50": [0.1, 0.2, 0.3]}}},
        }
        response = client.post("/positions/open", json=cloud_event_data)

        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR
        assert "Error getting cash balance" in response.json()["detail"]

        MockAlpacaClient.assert_called_once()
        mock_alpaca_instance.get_cash_balance.assert_called_once()

    @patch("application.positionmanager.src.positionmanager.main.AlpacaClient")
    def test_close_positions_success(self, MockAlpacaClient: MagicMock) -> None:  # noqa: N803
        mock_alpaca_instance = MagicMock(spec=AlpacaClient)
        mock_alpaca_instance.clear_positions.return_value = {
            "status": "success",
            "message": "All positions have been closed",
        }
        mock_cash_balance = Money(amount=Decimal("100000.00"))
        mock_alpaca_instance.get_cash_balance.return_value = mock_cash_balance
        MockAlpacaClient.return_value = mock_alpaca_instance

        response = client.post("/positions/close")

        assert response.status_code == status.HTTP_200_OK
        response_data = response.json()
        assert "source" in response_data
        assert "type" in response_data

        MockAlpacaClient.assert_called_once()
        mock_alpaca_instance.clear_positions.assert_called_once()
        mock_alpaca_instance.get_cash_balance.assert_called_once()

    @patch("application.positionmanager.src.positionmanager.main.AlpacaClient")
    def test_close_positions_error(
        self,
        MockAlpacaClient: MagicMock,  # noqa: N803
    ) -> None:
        mock_alpaca_instance = MagicMock(spec=AlpacaClient)
        mock_alpaca_instance.clear_positions.side_effect = HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Error getting cash balance",
        )
        MockAlpacaClient.return_value = mock_alpaca_instance

        response = client.post("/positions/close")

        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR
        assert "Error" in response.json()["detail"]

        MockAlpacaClient.assert_called_once()
        mock_alpaca_instance.clear_positions.assert_called_once()
