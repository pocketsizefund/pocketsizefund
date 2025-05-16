from fastapi.testclient import TestClient
import unittest
from unittest.mock import patch, MagicMock
import polars as pl
from application.positionmanager.src.positionmanager.main import application

client = TestClient(application)


def test_health_check():
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json() == {"status": "healthy"}


class TestPositionsEndpoint(unittest.TestCase):
    @patch("application.positionmanager.src.positionmanager.main.get_alpaca_client")
    @patch("application.positionmanager.src.positionmanager.main.get_data_client")
    @patch(
        "application.positionmanager.src.positionmanager.main.get_portfolio_optimizer"
    )
    def test_create_position_success(
        self, mock_get_optimizer, mock_get_data_client, mock_get_alpaca_client
    ):
        mock_alpaca_client = MagicMock()
        mock_alpaca_client.get_cash_balance.return_value = 100000.00
        mock_alpaca_client.place_notional_order.return_value = {
            "status": "success",
            "message": "Order placed successfully",
        }
        mock_get_alpaca_client.return_value = mock_alpaca_client

        mock_data_client = MagicMock()
        mock_historical_data = pl.DataFrame(
            {"date": ["2025-05-01"], "AAPL": [150.00], "MSFT": [250.00]}
        )
        mock_data_client.get_data.return_value = mock_historical_data
        mock_get_data_client.return_value = mock_data_client

        mock_optimizer = MagicMock()
        mock_optimizer.get_optimized_portfolio.return_value = {"AAPL": 10, "MSFT": 5}
        mock_get_optimizer.return_value = mock_optimizer

        payload = {"predictions": {"AAPL": {"signal": 0.8}}}
        response = client.post("/positions", json=payload)

        assert response.status_code == 200
        assert response.json()["status"] == "success"
        assert "initial_cash_balance" in response.json()
        assert "final_cash_balance" in response.json()
        assert "optimized_portfolio" in response.json()
        assert "executed_trades" in response.json()

        mock_get_alpaca_client.assert_called_once()
        mock_get_data_client.assert_called_once()
        mock_get_optimizer.assert_called_once()

        mock_alpaca_client.get_cash_balance.assert_called()
        mock_data_client.get_data.assert_called_once()
        mock_optimizer.get_optimized_portfolio.assert_called_once()
        assert mock_alpaca_client.place_notional_order.call_count == 2

    @patch("application.positionmanager.src.positionmanager.main.get_alpaca_client")
    def test_create_position_alpaca_error(self, mock_get_alpaca_client):
        mock_alpaca_client = MagicMock()
        mock_alpaca_client.get_cash_balance.side_effect = Exception("API error")
        mock_get_alpaca_client.return_value = mock_alpaca_client

        payload = {"predictions": {"AAPL": {"signal": 0.8}}}
        response = client.post("/positions", json=payload)

        assert response.status_code == 500
        assert "Error getting cash balance" in response.json()["detail"]

    @patch("application.positionmanager.src.positionmanager.main.get_alpaca_client")
    def test_delete_positions_success(self, mock_get_alpaca_client):
        mock_alpaca_client = MagicMock()
        mock_alpaca_client.clear_positions.return_value = {
            "status": "success",
            "message": "All positions have been closed",
        }
        mock_alpaca_client.get_cash_balance.return_value = 100000.00
        mock_get_alpaca_client.return_value = mock_alpaca_client

        response = client.delete("/positions")

        assert response.status_code == 200
        assert response.json()["status"] == "success"
        assert "cash_balance" in response.json()

        mock_get_alpaca_client.assert_called_once()
        mock_alpaca_client.clear_positions.assert_called_once()
        mock_alpaca_client.get_cash_balance.assert_called_once()

    @patch("application.positionmanager.src.positionmanager.main.get_alpaca_client")
    def test_delete_positions_error(self, mock_get_alpaca_client):
        mock_alpaca_client = MagicMock()
        mock_alpaca_client.clear_positions.side_effect = Exception("API error")
        mock_get_alpaca_client.return_value = mock_alpaca_client

        response = client.delete("/positions")

        assert response.status_code == 500
        assert "API error" in response.json()["detail"]
