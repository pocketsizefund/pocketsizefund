import httpx
import pytest

from application.datamanager.src.datamanager.main import application


@pytest.mark.integration
@pytest.mark.asyncio
async def test_health_endpoint():
    async with application.router.lifespan_context(application):
        async with httpx.AsyncClient(app=application, base_url="http://test") as client:
            response = await client.get("/health")
    assert response.status_code == 200
