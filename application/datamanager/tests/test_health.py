import httpx
import pytest
from starlette.testclient import TestClient

from application.datamanager.src.datamanager.main import application


@pytest.mark.integration
@pytest.mark.asyncio
async def test_health_endpoint():
    async with application.router.lifespan_context(application):
        async with httpx.AsyncClient(base_url="http://test") as client:
            response = await client.get("http://test/health")
    assert response.status_code == 200
