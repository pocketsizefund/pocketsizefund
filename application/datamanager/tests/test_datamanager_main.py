import unittest
from datetime import date
from unittest.mock import MagicMock, patch

from fastapi import status
from fastapi.testclient import TestClient

from application.datamanager.src.datamanager.main import application
from application.datamanager.src.datamanager.models import BarsSummary, SummaryDate

client = TestClient(application)


def test_health_check() -> None:
    response = client.get("/health")
    assert response.status_code == status.HTTP_200_OK


class TestDataManagerModels(unittest.TestCase):
    def test_summary_date_default(self) -> None:
        summary_date = SummaryDate()
        assert isinstance(summary_date.date, date)

    def test_summary_date_with_date(self) -> None:
        test_date = date(2023, 1, 1)
        summary_date = SummaryDate(date=test_date)
        assert summary_date.date == test_date

    def test_summary_date_string_parsing(self) -> None:
        summary_date = SummaryDate(date="2023-01-01")  # type: ignore
        assert summary_date.date == date(2023, 1, 1)

    def test_bars_summary_creation(self) -> None:
        bars_summary = BarsSummary(date="2023-01-01", count=100)
        assert bars_summary.date == "2023-01-01"
        assert bars_summary.count == 100  # noqa: PLR2004


class TestEquityBarsEndpoints(unittest.TestCase):
    def test_get_equity_bars_missing_parameters(self) -> None:
        response = client.get("/equity-bars")
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    def test_get_equity_bars_invalid_date_format(self) -> None:
        response = client.get(
            "/equity-bars",
            params={"start_date": "invalid-date", "end_date": "2023-01-02"},
        )
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    def test_post_equity_bars_missing_body(self) -> None:
        response = client.post("/equity-bars")
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    def test_post_equity_bars_invalid_date(self) -> None:
        response = client.post("/equity-bars", json={"date": "invalid-date"})
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    def test_delete_equity_bars_missing_body(self) -> None:
        response = client.request("DELETE", "/equity-bars")
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    def test_delete_equity_bars_invalid_date(self) -> None:
        response = client.request(
            "DELETE", "/equity-bars", json={"date": "invalid-date"}
        )
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    @patch("application.datamanager.src.datamanager.main.duckdb")
    def test_get_equity_bars_database_error(self, mock_duckdb: MagicMock) -> None:
        from duckdb import IOException

        mock_connection = MagicMock()
        mock_connection.execute.side_effect = IOException("Database error")
        mock_duckdb.connect.return_value = mock_connection

        mock_settings = MagicMock()
        mock_settings.gcp.bucket.name = "test-bucket"

        with patch.object(application, "state") as mock_app_state:
            mock_app_state.connection = mock_connection
            mock_app_state.settings = mock_settings

            response = client.get(
                "/equity-bars",
                params={"start_date": "2023-01-01", "end_date": "2023-01-02"},
            )

        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR


if __name__ == "__main__":
    unittest.main()
