from fastapi.testclient import TestClient
from application.datamanager.src.datamanager.main import application

client = TestClient(application)


def test_health_check():
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json() == {"status": "healthy"}
