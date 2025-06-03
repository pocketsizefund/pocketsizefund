import unittest
from decimal import Decimal
from unittest.mock import MagicMock, patch

import polars as pl
from fastapi import HTTPException, status
from fastapi.testclient import TestClient

from application.positionmanager.src.positionmanager.clients import (
    AlpacaClient,
    DataClient,
)
from application.positionmanager.src.positionmanager.main import application
from application.positionmanager.src.positionmanager.models import Money
from application.positionmanager.src.positionmanager.portfolio import PortfolioOptimizer

client = TestClient(application)


def test_health_check() -> None:
    response = client.get("/health")
    assert response.status_code == status.HTTP_200_OK
    assert response.json() == {"status": "healthy"}


class TestPositionsEndpoint(unittest.TestCase):
    @patch("application.positionmanager.src.positionmanager.main.AlpacaClient")
    @patch("application.positionmanager.src.positionmanager.main.DataClient")
    @patch("application.positionmanager.src.positionmanager.main.PortfolioOptimizer")
    def test_create_position_success(
        self,
        MockPortfolioOptimizer: MagicMock,  # noqa: N803
        MockDataClient: MagicMock,  # noqa: N803
        MockAlpacaClient: MagicMock,  # noqa: N803
    ) -> None:
        mock_alpaca_instance = MagicMock(spec=AlpacaClient)
        mock_cash_balance = Money(amount=Decimal("100000.00"))
        mock_alpaca_instance.get_cash_balance.return_value = mock_cash_balance
        mock_alpaca_instance.place_notional_order.return_value = {
            "status": "success",
            "message": "Order placed successfully",
        }
        MockAlpacaClient.return_value = mock_alpaca_instance

        mock_data_instance = MagicMock(spec=DataClient)
        mock_historical_data = pl.DataFrame(
            {"date": ["2025-05-01"], "AAPL": [150.00], "MSFT": [250.00]},
        )
        mock_data_instance.get_data.return_value = mock_historical_data
        MockDataClient.return_value = mock_data_instance

        mock_optimizer_instance = MagicMock(spec=PortfolioOptimizer)
        mock_optimizer_instance.get_optimized_portfolio.return_value = {
            "AAPL": 10,
            "MSFT": 5,
        }
        MockPortfolioOptimizer.return_value = mock_optimizer_instance

        payload = {"predictions": {"AAPL": 0.8, "MSFT": 0.7}}
        response = client.post("/positions", json=payload)

        assert response.status_code == status.HTTP_200_OK
        assert response.json()["status"] == "success"
        assert "initial_cash_balance" in response.json()
        assert "final_cash_balance" in response.json()
        assert "optimized_portfolio" in response.json()
        assert "executed_trades" in response.json()

        MockAlpacaClient.assert_called_once()
        MockDataClient.assert_called_once()
        MockPortfolioOptimizer.assert_called_once()

        mock_alpaca_instance.get_cash_balance.assert_called()
        mock_data_instance.get_data.assert_called_once()

        mock_optimizer_instance.get_optimized_portfolio.assert_called_once()
        optimizer_args = mock_optimizer_instance.get_optimized_portfolio.call_args[1]
        assert "predictions" in optimizer_args
        assert "portfolio_value" in optimizer_args
        assert optimizer_args["portfolio_value"] == mock_cash_balance

        assert mock_alpaca_instance.place_notional_order.call_count == 2  # noqa: PLR2004

    @patch("application.positionmanager.src.positionmanager.main.AlpacaClient")
    def test_create_position_alpaca_error(self, MockAlpacaClient: MagicMock) -> None:  # noqa: N803
        mock_alpaca_instance = MagicMock(spec=AlpacaClient)
        mock_alpaca_instance.get_cash_balance.side_effect = HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Error getting cash balance",
        )
        MockAlpacaClient.return_value = mock_alpaca_instance

        payload = {"predictions": {"AAPL": 0.8}}
        response = client.post("/positions", json=payload)

        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR
        assert "Error getting cash balance" in response.json()["detail"]

        MockAlpacaClient.assert_called_once()
        mock_alpaca_instance.get_cash_balance.assert_called_once()

    @patch("application.positionmanager.src.positionmanager.main.AlpacaClient")
    def test_delete_positions_success(self, MockAlpacaClient: MagicMock) -> None:  # noqa: N803
        mock_alpaca_instance = MagicMock(spec=AlpacaClient)
        mock_alpaca_instance.clear_positions.return_value = {
            "status": "success",
            "message": "All positions have been closed",
        }
        mock_cash_balance = Money(amount=Decimal("100000.00"))
        mock_alpaca_instance.get_cash_balance.return_value = mock_cash_balance
        MockAlpacaClient.return_value = mock_alpaca_instance

        response = client.delete("/positions")

        assert response.status_code == status.HTTP_200_OK
        assert response.json()["status"] == "success"
        assert "cash_balance" in response.json()
        assert response.json()["cash_balance"] == float(mock_cash_balance)

        MockAlpacaClient.assert_called_once()
        mock_alpaca_instance.clear_positions.assert_called_once()
        mock_alpaca_instance.get_cash_balance.assert_called_once()

    @patch("application.positionmanager.src.positionmanager.main.AlpacaClient")
    def test_delete_positions_error(
        self,
        MockAlpacaClient: MagicMock,  # noqa: N803
    ) -> None:
        mock_alpaca_instance = MagicMock(spec=AlpacaClient)
        mock_alpaca_instance.clear_positions.side_effect = HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Error getting cash balance",
        )
        MockAlpacaClient.return_value = mock_alpaca_instance

        response = client.delete("/positions")

        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR
        assert "Error" in response.json()["detail"]

        MockAlpacaClient.assert_called_once()
        mock_alpaca_instance.clear_positions.assert_called_once()
