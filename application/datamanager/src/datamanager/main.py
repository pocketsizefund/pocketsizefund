import json
import os
from contextlib import asynccontextmanager
from datetime import date, datetime, timedelta
from pathlib import Path

import httpx
import duckdb
import polars as pl
from fastapi import FastAPI, HTTPException, Request, Response, status
from .config import Settings
from .models import BarsResult, DateRange
from .query import list_file_paths, query_bars
from loguru import logger


def get_connection(settings: Settings) -> duckdb.DuckDBPyConnection:
    connection = duckdb.connect()
    if settings.gcp_key_id and settings.gcp_secret:
        connection.execute("INSTALL httpfs;")
        connection.execute("LOAD httpfs;")
        connection.execute(
            f"""CREATE SECRET (TYPE gcs, KEY_ID '{settings.gcp_key_id}', SECRET '{settings.gcp_secret}');"""
        )
    return connection


def delete_bars(date: date, bucket: str):
    path = f"gs://{bucket}/equity/bars/{date.strftime('%Y-%m-%d')}/data.parquet"


@asynccontextmanager
async def lifespan(app: FastAPI):
    app.state.settings = Settings()

    yield


application = FastAPI(lifespan=lifespan)


@application.get("/health")
async def health_check():
    return Response(status_code=status.HTTP_200_OK)


@application.get("/equity-bars")
async def get_equity_bars(
    request: Request,
    date_range: DateRange,
) -> dict[str, object]:
    settings: Settings = request.app.state.settings
    paths = await list_file_paths(
        bucket=settings.data_bucket, date_range=settings.date_range
    )
    results = await query_bars(filepaths=paths)
    logger.info(results.head())

    return {
        "data": results.to_dicts(),
        "metadata": {
            "start_date": str(date_range.start),
            "end_date": str(date_range.end),
            "count": len(results),
        },
    }


@application.post("/equity-bars", response_model=BarsResult)
async def fetch_equity_bars(request: Request, date: date | None = None) -> BarsResult:
    settings: Settings = request.app.state.settings
    target_date = (date or datetime.utcnow()).date()
    params = {"adjusted": "true", "apiKey": settings.polygon.api_key}
    async with httpx.AsyncClient() as client:
        response = await client.get(
            f"{settings.polygon.base_url}{settings.polygon.daily_bars}{target_date}",
            params=params,
        )
    response.raise_for_status()
    payload = response.json().get("results", [])

    bars = pl.DataFrame(payload)
    logger.info(bars.head())

    # logger.info(f"storing dataframe to {path=}")

    bars.write_parquet(settings.bucket.daily_bars_path(target_date))
    return BarsResult(date=str(target_date), count=len(bars))


@application.delete("/equity-bars")
async def delete_equity_bars(request: Request, date: date):
    settings: Settings = request.app.state.settings
    success = delete_bars(date, settings)

    if not success:
        return Response(
            status_code=status.HTTP_404_NOT_FOUND,
            content=json.dumps({"detail": f"No data found for date {date}"}),
            media_type="application/json",
        )

    return Response(status_code=status.HTTP_204_NO_CONTENT)
