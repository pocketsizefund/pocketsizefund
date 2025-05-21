from fastapi.testclient import TestClient
from application.accountmanager.src.accountmanager.main import application

client = TestClient(application)


def test_health_check():
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json() == {"status": "healthy"}


def test_get_account(monkeypatch):
    mock_account = type("Account", (), {"model_dump": lambda self: {"status": "ok"}})()

    class MockClient:
        def get_account(self):
            return mock_account

    monkeypatch.setattr(
        "application.accountmanager.src.accountmanager.main.get_client",
        lambda: MockClient(),
    )

    response = client.get("/account")
    assert response.status_code == 200
    assert response.json() == {"status": "ok"}
