import unittest
from datetime import date
from unittest.mock import MagicMock, patch

from duckdb import IOException
from fastapi import FastAPI, status
from fastapi.testclient import TestClient

from application.datamanager.src.datamanager.main import application, get_metrics
from application.datamanager.src.datamanager.models import SummaryDate

client = TestClient(application)


def test_get_health() -> None:
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


class TestMetricsEndpoint(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.test_app = FastAPI()
        cls.test_app.get("/metrics")(get_metrics)
        cls.test_client = TestClient(cls.test_app)

    def test_get_metrics_success(self) -> None:
        mock_connection = MagicMock()
        mock_connection.execute.return_value.fetchone.return_value = (1000,)

        mock_s3_client = MagicMock()
        mock_s3_client.data_bucket_name = "test-bucket"

        self.test_app.state.connection = mock_connection
        self.test_app.state.s3_client = mock_s3_client

        response = self.test_client.get("/metrics")

        assert response.status_code == status.HTTP_200_OK
        response_data = response.json()
        assert response_data["total_rows"] == 1000  # noqa: PLR2004

    def test_get_metrics_database_error(self) -> None:
        mock_connection = MagicMock()
        mock_connection.execute.side_effect = IOException("Database error")

        mock_s3_client = MagicMock()
        mock_s3_client.data_bucket_name = "test-bucket"

        self.test_app.state.connection = mock_connection
        self.test_app.state.s3_client = mock_s3_client

        response = self.test_client.get("/metrics")

        mock_connection.execute.assert_called_once()
        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR
        response_data = response.json()
        assert "error" in response_data
        assert response_data["error"] == "Failed to fetch metrics"


class TestEquityBarsEndpoints(unittest.TestCase):
    def test_get_equity_bars_missing_parameters(self) -> None:
        response = client.get("/equity-bars")
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    def test_get_equity_bars_invalid_date_format(self) -> None:
        response = client.get(
            url="/equity-bars",
            params={
                "start_date": "invalid-date",
                "end_date": "2023-01-02",
            },
        )
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    @patch("application.datamanager.src.datamanager.main.duckdb")
    def test_get_equity_bars_database_error(self, mock_duckdb: MagicMock) -> None:
        mock_connection = MagicMock()
        mock_connection.execute.side_effect = IOException("Database error")
        mock_duckdb.connect.return_value = mock_connection

        mock_s3_client = MagicMock()
        mock_s3_client.data_bucket_name = "test-bucket"

        with patch.object(application, "state") as mock_app_state:
            mock_app_state.connection = mock_connection
            mock_app_state.s3_client = mock_s3_client

            response = client.get(
                url="/equity-bars",
                params={
                    "start_date": "2023-01-01",
                    "end_date": "2023-01-02",
                },
            )

        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR

    @patch("application.datamanager.src.datamanager.main.duckdb")
    def test_get_equity_bars_no_data(self, mock_duckdb: MagicMock) -> None:
        mock_connection = MagicMock()
        mock_arrow_result = MagicMock()
        mock_arrow_result.num_rows = 0
        mock_connection.execute.return_value.arrow.return_value = mock_arrow_result
        mock_duckdb.connect.return_value = mock_connection

        mock_s3_client = MagicMock()
        mock_s3_client.data_bucket_name = "test-bucket"

        with patch.object(application, "state") as mock_app_state:
            mock_app_state.connection = mock_connection
            mock_app_state.s3_client = mock_s3_client

            response = client.get(
                url="/equity-bars",
                params={
                    "start_date": "2023-01-01",
                    "end_date": "2023-01-02",
                },
            )

        assert response.status_code == status.HTTP_404_NOT_FOUND
        response_data = response.json()
        assert "error" in response_data

    def test_delete_equity_bars_missing_body(self) -> None:
        response = client.request("DELETE", "/equity-bars")
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    def test_delete_equity_bars_invalid_date(self) -> None:
        response = client.request(
            method="DELETE",
            url="/equity-bars",
            json={
                "date": "invalid-date",
            },
        )
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    @patch("application.datamanager.src.datamanager.main.S3Client")
    def test_delete_equity_bars_success(self, mock_s3_client_class: MagicMock) -> None:
        mock_s3_client = MagicMock()
        mock_s3_client.list_objects.return_value = ["file1.parquet", "file2.parquet"]
        mock_s3_client_class.return_value = mock_s3_client

        with patch.object(application, "state") as mock_app_state:
            mock_app_state.s3_client = mock_s3_client

            response = client.request(
                method="DELETE",
                url="/equity-bars",
                json={"date": "2023-01-01"},
            )

        assert response.status_code == status.HTTP_204_NO_CONTENT
        mock_s3_client.delete_objects.assert_called_once()

    @patch("application.datamanager.src.datamanager.main.S3Client")
    def test_delete_equity_bars_not_found(
        self, mock_s3_client_class: MagicMock
    ) -> None:
        mock_s3_client = MagicMock()
        mock_s3_client.list_objects.return_value = []
        mock_s3_client_class.return_value = mock_s3_client

        with patch.object(application, "state") as mock_app_state:
            mock_app_state.s3_client = mock_s3_client

            response = client.request(
                method="DELETE",
                url="/equity-bars",
                json={"date": "2023-01-01"},
            )

        assert response.status_code == status.HTTP_404_NOT_FOUND
        response_data = response.json()
        assert "error" in response_data


class TestFetchEquityBarsEndpoint(unittest.TestCase):
    @patch("application.datamanager.src.datamanager.main.PolygonClient")
    @patch("application.datamanager.src.datamanager.main.pl.DataFrame")
    def test_fetch_equity_bars_success(
        self,
        mock_dataframe: MagicMock,
        mock_polygon_client_class: MagicMock,
    ) -> None:
        mock_polygon_client = MagicMock()
        mock_polygon_client.get_all_equity_bars.return_value = [{"test": "data"}]
        mock_polygon_client_class.return_value = mock_polygon_client

        mock_df = MagicMock()
        mock_df.__len__.return_value = 1
        mock_df.with_columns.return_value = mock_df
        mock_dataframe.return_value = mock_df

        mock_s3_client = MagicMock()
        mock_s3_client.daily_equity_bars_path = "s3://test-bucket/equity/bars/"

        with patch.object(application, "state") as mock_app_state:
            mock_app_state.polygon_client = mock_polygon_client
            mock_app_state.s3_client = mock_s3_client

            response = client.post("/equity-bars/fetch", json={"date": "2023-01-01"})

        assert response.status_code == status.HTTP_200_OK
        response_data = response.json()
        assert "source" in response_data
        assert "type" in response_data
        assert response_data["type"] == "application.datamanager.equity.bars.created"

    def test_fetch_equity_bars_invalid_date(self) -> None:
        response = client.post("/equity-bars/fetch", json={"date": "invalid-date"})
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    @patch("application.datamanager.src.datamanager.main.PolygonClient")
    @patch("application.datamanager.src.datamanager.main.pl.DataFrame")
    def test_fetch_equity_bars_write_error(
        self,
        mock_dataframe: MagicMock,
        mock_polygon_client_class: MagicMock,
    ) -> None:
        mock_polygon_client = MagicMock()
        mock_polygon_client.get_all_equity_bars.return_value = [{"test": "data"}]
        mock_polygon_client_class.return_value = mock_polygon_client

        mock_df = MagicMock()
        mock_df.__len__.return_value = 1
        mock_df.with_columns.return_value = mock_df
        mock_df.write_parquet.side_effect = IOException("Write error")
        mock_dataframe.return_value = mock_df

        mock_s3_client = MagicMock()
        mock_s3_client.daily_equity_bars_path = "s3://test-bucket/equity/bars/"

        with patch.object(application, "state") as mock_app_state:
            mock_app_state.polygon_client = mock_polygon_client
            mock_app_state.s3_client = mock_s3_client

            response = client.post("/equity-bars/fetch", json={"date": "2023-01-01"})

        assert response.status_code == status.HTTP_200_OK
        response_data = response.json()
        assert response_data["type"] == "application.datamanager.equity.bars.errored"
        assert "error" in response_data["data"]


if __name__ == "__main__":
    unittest.main()
