import traceback
import pyarrow
import os
from prometheus_fastapi_instrumentator import Instrumentator

import duckdb
from google.api_core import exceptions
from google.cloud import storage
from contextlib import asynccontextmanager
from datetime import date
import httpx
import polars as pl
from fastapi import FastAPI, Request, Response, status, HTTPException
from .config import Settings
from .models import BarsSummary, SummaryDate
from loguru import logger


def bars_query(*, bucket: str, start_date: date, end_date: date) -> str:
    path_pattern = f"gs://{bucket}/equity/bars/*/*/*/*"

    return f"""
        SELECT *
        FROM read_parquet(
            '{path_pattern}', 
            HIVE_PARTITIONING=1
        )
        WHERE 
            (year > {start_date.year} OR 
             (year = {start_date.year} AND month > {start_date.month}) OR 
             (year = {start_date.year} AND month = {start_date.month} AND day >= {start_date.day}))
            AND
            (year < {end_date.year} OR 
             (year = {end_date.year} AND month < {end_date.month}) OR 
             (year = {end_date.year} AND month = {end_date.month} AND day <= {end_date.day}))
    """


@asynccontextmanager
async def lifespan(app: FastAPI):
    app.state.settings = Settings()
    app.state.bucket = storage.Client(os.getenv("GCP_PROJECT")).bucket(
        app.state.settings.gcp.bucket.name
    )

    DUCKDB_ACCESS_KEY = os.getenv("DUCKDB_ACCESS_KEY")
    DUCKDB_SECRET = os.getenv("DUCKDB_SECRET")

    app.state.connection = duckdb.connect()
    app.state.connection.execute(f"""
      INSTALL httpfs;
      LOAD httpfs;
      SET http_keep_alive=true;
      SET http_timeout=30000;
      CREATE SECRET (
        TYPE GCS, 
        KEY_ID '{DUCKDB_ACCESS_KEY}',
        SECRET '{DUCKDB_SECRET}'
      );
    """)

    yield


application = FastAPI(lifespan=lifespan)
Instrumentator().instrument(application).expose(application)


@application.get("/health")
async def health_check():
    return Response(status_code=status.HTTP_200_OK)


@application.get("/equity-bars")
async def get_equity_bars(
    request: Request, start_date: date, end_date: date
) -> Response:
    settings: Settings = request.app.state.settings

    query = bars_query(
        bucket=settings.gcp.bucket.name, start_date=start_date, end_date=end_date
    )

    try:
        data = request.app.state.connection.execute(query).arrow()

        if data.num_rows == 0:
            return Response(status_code=status.HTTP_404_NOT_FOUND)

        logger.info(f"Query returned {data.num_rows} rows")
        sink = pyarrow.BufferOutputStream()
        with pyarrow.ipc.RecordBatchStreamWriter(sink, data.schema) as writer:
            writer.write_table(data)

        return Response(
            content=sink.getvalue().to_pybytes(),
            media_type="application/vnd.apache.arrow.file",
            headers={
                "Content-Disposition": f"attachment; filename=equity_bars_{start_date}_{end_date}.arrow",
                "X-Row-Count": str(data.num_rows),
                "X-Start-Date": str(start_date),
                "X-End-Date": str(end_date),
            },
        )

    except Exception as e:
        logger.error(f"Error querying data: {e}")
        logger.error(traceback.format_exc())
        return Response(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR)


@application.post("/equity-bars", response_model=BarsSummary)
async def fetch_equity_bars(request: Request, summary_date: SummaryDate) -> BarsSummary:
    polygon = request.app.state.settings.polygon
    bucket = request.app.state.settings.gcp.bucket

    url = f"{polygon.base_url}{polygon.daily_bars}{summary_date.date.strftime('%Y-%m-%d')}"
    logger.info(f"polygon_api_endpoint={url}")

    params = {"adjusted": "true", "apiKey": polygon.api_key}
    async with httpx.AsyncClient() as client:
        response = await client.get(
            url,
            params=params,
        )
    response.raise_for_status()
    payload = response.json().get("results", [])

    bars = pl.DataFrame(payload)
    count = len(bars)
    if count > 0:
        try:
            bars.with_columns(
                [
                    pl.from_epoch("t", time_unit="ms").alias("datetime"),
                    pl.from_epoch("t", time_unit="ms").dt.year().alias("year"),
                    pl.from_epoch("t", time_unit="ms").dt.month().alias("month"),
                    pl.from_epoch("t", time_unit="ms").dt.day().alias("day"),
                ]
            ).write_parquet(
                bucket.daily_bars_path, partition_by=["year", "month", "day"]
            )
        except Exception as e:
            logger.error(f"Error writing parquet file: {e}")
            logger.error(traceback.format_exc())
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail="Failed to write data",
            )
    return BarsSummary(date=summary_date.date.strftime("%Y-%m-%d"), count=count)


@application.delete("/equity-bars")
async def delete_equity_bars(request: Request, summary_date: SummaryDate):
    bucket = request.app.state.bucket
    year = summary_date.date.year
    month = summary_date.date.month
    day = summary_date.date.day
    prefix = f"equity/bars/{year=}/{month=}/{day=}"

    try:
        blobs = list(bucket.list_blobs(prefix=prefix))
    except exceptions.NotFound:
        return Response(status_code=status.HTTP_404_NOT_FOUND)
    if not blobs:
        return Response(status_code=status.HTTP_404_NOT_FOUND)

    logger.info(f"deleting {prefix=}")
    bucket.delete_blobs(blobs)

    return Response(status_code=status.HTTP_204_NO_CONTENT)
