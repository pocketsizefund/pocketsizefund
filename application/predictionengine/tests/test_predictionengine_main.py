import unittest
from unittest.mock import MagicMock, patch

from fastapi import status
from fastapi.testclient import TestClient

from application.predictionengine.src.predictionengine.main import application

client = TestClient(application)


def test_health_check() -> None:
    response = client.get("/health")
    assert response.status_code == status.HTTP_200_OK


class TestCreatePredictionsEndpoint(unittest.TestCase):
    @patch("application.predictionengine.src.predictionengine.main.requests.get")
    def test_create_predictions_success(
        self,
        mock_requests_get: MagicMock,
    ) -> None:
        mock_response = MagicMock()
        mock_response.content = b"mock_arrow_data"
        mock_response.raise_for_status.return_value = None
        mock_requests_get.return_value = mock_response

        with (
            patch(
                "application.predictionengine.src.predictionengine.main.fetch_historical_data"
            ) as mock_fetch_data,
            patch(
                "application.predictionengine.src.predictionengine.main.load_or_initialize_model"
            ) as mock_load_model,
            patch(
                "application.predictionengine.src.predictionengine.main.get_predictions"
            ) as mock_get_predictions,
            patch.object(application, "state") as mock_app_state,
        ):
            mock_app_state.datamanager_base_url = "http://test-datamanager"
            mock_app_state.model = None

            mock_df = MagicMock()
            mock_df.is_empty.return_value = False
            mock_df.unique.return_value.to_list.return_value = ["AAPL", "MSFT"]
            mock_fetch_data.return_value = mock_df

            mock_model = MagicMock()
            mock_load_model.return_value = mock_model

            mock_get_predictions.return_value = {
                "AAPL": {
                    "percentile_25": 0.7,
                    "percentile_50": 0.8,
                    "percentile_75": 0.9,
                },
                "MSFT": {
                    "percentile_25": 0.6,
                    "percentile_50": 0.75,
                    "percentile_75": 0.85,
                },
            }

            response = client.post("/predictions/create")

        assert response.status_code == status.HTTP_200_OK
        response_data = response.json()
        assert "source" in response_data
        assert "type" in response_data
        assert (
            response_data["type"] == "application.predictionengine.predictions.created"
        )
        assert "data" in response_data
        assert "AAPL" in response_data["data"]["predictions"]
        assert "MSFT" in response_data["data"]["predictions"]


if __name__ == "__main__":
    unittest.main()
