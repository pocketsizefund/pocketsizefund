import os
from typing import Any

import httpx
from fastapi import FastAPI, HTTPException, Response, status

app: FastAPI = FastAPI()

DATAMANAGER_URL = os.getenv("DATAMANAGER_URL", "http://datamanager:8080")
HTTP_200_OK = 200


@app.get("/health")
def health_check() -> Response:
    return Response(status_code=status.HTTP_200_OK)


@app.get("/datamanager/health")
async def check_datamanager_health() -> dict[str, Any]:
    """Check if datamanager service is healthy"""
    try:
        async with httpx.AsyncClient() as client:
            response = await client.get(
                f"{DATAMANAGER_URL}/portfolio-check", timeout=5.0
            )
            return {
                "datamanager_status": "healthy"
                if response.status_code == HTTP_200_OK
                else "unhealthy",
                "status_code": response.status_code,
            }
    except httpx.RequestError as e:
        raise HTTPException(
            status_code=503, detail=f"Cannot reach datamanager: {e!s}"
        ) from e


application = app
