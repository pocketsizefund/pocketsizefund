from __future__ import annotations

import os
from contextlib import asynccontextmanager
from datetime import date, datetime, timedelta
from pathlib import Path

import httpx
import duckdb
import polars as pl
from fastapi import FastAPI, HTTPException, Request
from pydantic import BaseModel

from config import Settings
from models import BarsResult, DateRange


def get_connection(settings: Settings) -> duckdb.DuckDBPyConnection:
    connection = duckdb.connect()
    if settings.gcp_key_id and settings.gcp_secret:
        connection.execute("INSTALL httpfs;")
        connection.execute("LOAD httpfs;")
        connection.execute(
            f"""CREATE SECRET (TYPE gcs, KEY_ID '{settings.gcp_key_id}', SECRET '{settings.gcp_secret}');"""
        )
    return connection


def store_bars(data: pl.DataFrame, date: date, settings: Settings) -> None:
    path = (
        f"gs://{settings.gcp_gcs_bucket}/equity/bars/{date.strftime('%Y-%m-%d')}/data.parquet"
        if settings.gcp_gcs_bucket
        else f"equity_bars_{date.strftime('%Y-%m-%d')}.parquet"
    )
    connection = get_connection(settings)
    connection.register("data_frame", data)
    connection.execute(f"COPY data_frame TO '{path}' (FORMAT PARQUET)")


def query_bars(
    request: Request, date_range: DateRange, settings: Settings
) -> pl.DataFrame:
    filepaths = []
    current = date_range.start
    while current <= date_range.end:
        filepaths.append(
            (
                f"gs://{settings.gcp_gcs_bucket}/equity/bars/{current.strftime('%Y-%m-%d')}/data.parquet"
                if settings.gcp_gcs_bucket
                else f"equity_bars_{current.strftime('%Y-%m-%d')}.parquet"
            )
        )
        current += timedelta(days=1)
    sql = request.app.state.SQL_PATH.read_text().format(filepaths=str(filepaths))
    connection = get_connection(settings)
    return connection.execute(sql).pl()


@asynccontextmanager
def lifespan(app: FastAPI):
    app.state.settings = Settings(
        polygon_api_key=os.getenv("POLYGON_API_KEY"),
        polygon_base_url=os.getenv("POLYGON_BASE_URL", "https://api.polygon.io"),
        gcp_key_id=os.getenv("GCP_KEY_ID"),
        gcp_secret=os.getenv("GCP_SECRET"),
        gcp_gcs_bucket=os.getenv("GCP_GCS_BUCKET"),
    )

    app.state.SQL_PATH = Path(__file__).with_name("bars.sql")
    yield


application = FastAPI(lifespan=lifespan)


@application.get("/health")
async def get_health() -> dict[str, str]:
    return {"status": "healthy"}


@application.get("/equity-bars")
async def get_equity_bars(
    request: Request, start_date: datetime, end_date: datetime
) -> dict[str, object]:
    settings: Settings = request.app.state.settings
    results = query_bars(start_date, end_date, settings)
    return {
        "data": results.to_dicts(),
        "metadata": {
            "start_date": start_date,
            "end_date": end_date,
            "count": len(results),
        },
    }


@application.post("/equity-bars", response_model=BarsResult)
async def fetch_equity_bars(request: Request, date: date | None = None) -> BarsResult:
    settings: Settings = request.app.state.settings
    target_date = (date or datetime.utcnow()).date()
    if not settings.polygon_api_key:
        raise HTTPException(status_code=500, detail="Missing API key")
    url = f"{settings.polygon_base_url}/v2/aggs/grouped/locale/us/market/stocks/{target_date}"
    params = {"adjusted": "true", "apiKey": settings.polygon_api_key}
    async with httpx.AsyncClient() as client:
        response = await client.get(url, params=params)
    response.raise_for_status()
    payload = response.json().get("results", [])
    frame = pl.DataFrame(payload)
    store_bars(frame, datetime.combine(target_date, datetime.min.time()), settings)
    return BarsResult(date=str(target_date), count=len(frame))
