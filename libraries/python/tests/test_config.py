"""Test api key required decorator."""

import os

from flask import Flask

from pocketsizefund.config.config import api_key_required


def test_api_key_required_all_keys_present() -> None:
    """Test api key required decorator with all keys present."""
    os.environ.clear()
    env_vars = {
        "DARQUBE_API_KEY": "test_key1",
        "ALPACA_API_KEY": "test_key2",
        "ALPACA_API_SECRET": "test_secret",
        "ALPHA_VANTAGE_API_KEY": "test_key3",
        "EDGAR_USER_AGENT": "test_agent",
        "MODEL_FILE_NAME": "test_model.pkl",
    }

    for key, value in env_vars.items():
        os.environ[key] = value

    app = Flask(__name__)

    client = app.test_client()

    @api_key_required
    @app.get("/")
    def dummy_function() -> str:
        return "Success"

    response = client.get("/")

    assert response.status_code == 200
    assert response.data == b"Success"


def test_api_key_required_missing_key() -> None:
    """Test api key required decorator with missing key."""
    env_vars = {
        "DARQUBE_API_KEY": "test_key1",
        "ALPACA_API_KEY": "test_key2",
        "ALPACA_API_SECRET": "test_secret",
        "ALPHA_VANTAGE_API_KEY": "test_key3",
        "EDGAR_USER_AGENT": "test_agent",
        # MODEL_FILE_NAME is missing
    }

    os.environ.clear()
    for key, value in env_vars.items():
        os.environ[key] = value

    app = Flask(__name__)

    client = app.test_client()

    @app.get("/")
    @api_key_required
    def dummy_function() -> str:
        return "Success"

    response = client.get("/")

    assert response.status_code == 401


def test_environment_empty() -> None:
    """Test api key required decorator with empty environment."""
    os.environ.clear()

    app = Flask(__name__)

    client = app.test_client()

    @app.get("/")
    @api_key_required
    def dummy_function() -> str:
        return "Success"

    response = client.get("/")

    assert response.status_code == 401


def test_api_key_required_extra_key() -> None:
    """Test api key required decorator with extra, unrequired key."""
    env_vars = {
        "DARQUBE_API_KEY": "test_key1",
        "ALPACA_API_KEY": "test_key2",
        "ALPACA_API_SECRET": "test_secret",
        "ALPHA_VANTAGE_API_KEY": "test_key3",
        "EDGAR_USER_AGENT": "test_agent",
        "MODEL_FILE_NAME": "test_model.pkl",
        "EXTRA_KEY": "extra_value",
    }

    for key, value in env_vars.items():
        os.environ[key] = value

    assert os.getenv("EXTRA_KEY") == "extra_value"

    app = Flask(__name__)

    client = app.test_client()

    @app.get("/")
    @api_key_required
    def dummy_function() -> str:
        return "Success"

    response = client.get("/")

    assert response.status_code == 200
    assert response.data == b"Success"
