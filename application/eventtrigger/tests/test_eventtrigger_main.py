import unittest
from unittest.mock import MagicMock, patch

import requests
from fastapi import status
from fastapi.testclient import TestClient

from application.eventtrigger.src.eventtrigger.main import application

client = TestClient(application)


def test_health_check() -> None:
    response = client.get("/health")
    assert response.status_code == status.HTTP_200_OK


class TestTriggerEventEndpoint(unittest.TestCase):
    def test_trigger_event_missing_body(self) -> None:
        response = client.post("/trigger")
        assert response.status_code == status.HTTP_422_UNPROCESSABLE_ENTITY

    def test_trigger_event_invalid_event_type(self) -> None:
        response = client.post("/trigger", json={"event": "invalid_event"})
        assert response.status_code == status.HTTP_400_BAD_REQUEST

    @patch("application.eventtrigger.src.eventtrigger.main.os.getenv")
    @patch("application.eventtrigger.src.eventtrigger.main.requests.post")
    def test_trigger_event_fetch_data_success(
        self, mock_post: MagicMock, mock_getenv: MagicMock
    ) -> None:
        mock_getenv.return_value = "https://datamanager.example.com"
        mock_response = MagicMock()
        mock_response.raise_for_status.return_value = None
        mock_post.return_value = mock_response

        response = client.post("/trigger", json={"event": "fetch_data"})

        assert response.status_code == status.HTTP_200_OK
        mock_post.assert_called_once()
        call_args = mock_post.call_args
        assert "https://datamanager.example.com/equity-bars" in str(call_args[0][0])
        assert "json" in call_args[1]
        assert "date" in call_args[1]["json"]

    @patch("application.eventtrigger.src.eventtrigger.main.os.getenv")
    @patch("application.eventtrigger.src.eventtrigger.main.requests.post")
    def test_trigger_event_create_positions_success(
        self, mock_post: MagicMock, mock_getenv: MagicMock
    ) -> None:
        mock_getenv.return_value = "https://predictionengine.example.com"
        mock_response = MagicMock()
        mock_response.raise_for_status.return_value = None
        mock_post.return_value = mock_response

        response = client.post("/trigger", json={"event": "create_positions"})

        assert response.status_code == status.HTTP_200_OK
        mock_post.assert_called_once()
        call_args = mock_post.call_args
        assert "https://predictionengine.example.com/create-positions" in str(
            call_args[0][0]
        )

    @patch("application.eventtrigger.src.eventtrigger.main.os.getenv")
    @patch("application.eventtrigger.src.eventtrigger.main.requests.delete")
    def test_trigger_event_close_positions_success(
        self, mock_delete: MagicMock, mock_getenv: MagicMock
    ) -> None:
        mock_getenv.return_value = "https://positionmanager.example.com"
        mock_response = MagicMock()
        mock_response.raise_for_status.return_value = None
        mock_delete.return_value = mock_response

        response = client.post("/trigger", json={"event": "close_positions"})

        assert response.status_code == status.HTTP_200_OK
        mock_delete.assert_called_once()
        call_args = mock_delete.call_args
        assert "https://positionmanager.example.com/positions" in str(call_args[0][0])

    @patch("application.eventtrigger.src.eventtrigger.main.os.getenv")
    @patch("application.eventtrigger.src.eventtrigger.main.requests.post")
    def test_trigger_event_request_exception(
        self, mock_post: MagicMock, mock_getenv: MagicMock
    ) -> None:
        mock_getenv.return_value = "https://datamanager.example.com"
        mock_post.side_effect = requests.exceptions.ConnectionError("Connection failed")

        response = client.post("/trigger", json={"event": "fetch_data"})

        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR
        assert "Error triggering event" in response.text

    @patch("application.eventtrigger.src.eventtrigger.main.os.getenv")
    @patch("application.eventtrigger.src.eventtrigger.main.requests.post")
    def test_trigger_event_http_error(
        self, mock_post: MagicMock, mock_getenv: MagicMock
    ) -> None:
        mock_getenv.return_value = "https://datamanager.example.com"
        mock_response = MagicMock()
        mock_response.raise_for_status.side_effect = requests.exceptions.HTTPError(
            "HTTP Error"
        )
        mock_post.return_value = mock_response

        response = client.post("/trigger", json={"event": "fetch_data"})

        assert response.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR
        assert "Error triggering event" in response.text


if __name__ == "__main__":
    unittest.main()
