import pytest


@pytest.fixture(scope="session")
def base_url():
    return "http://localhost:8000"
