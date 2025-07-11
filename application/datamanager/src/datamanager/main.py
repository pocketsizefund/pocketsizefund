import json
import os
import traceback
from collections.abc import AsyncGenerator
from contextlib import asynccontextmanager
from datetime import date, datetime
from zoneinfo import ZoneInfo

import duckdb
import polars as pl
import pyarrow as pa
import pyarrow.lib
import requests
from cloudevents.pydantic.v2 import CloudEvent
from duckdb import IOException
from fastapi import FastAPI, Request, Response, status
from google.api_core import exceptions
from google.api_core.exceptions import GoogleAPIError
from loguru import logger
from polars.exceptions import ComputeError
from prometheus_client import Gauge
from prometheus_fastapi_instrumentator import Instrumentator

from .clients import PolygonClient, S3Client
from .models import SummaryDate


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
             (year = {start_date.year} AND month = {start_date.month}
               AND day >= {start_date.day}))
            AND
            (year < {end_date.year} OR 
             (year = {end_date.year} AND month < {end_date.month}) OR 
             (year = {end_date.year}
               AND month = {end_date.month}
               AND day <= {end_date.day}))
    """  # noqa: S608


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None, None]:
    app.state.polygon_client = PolygonClient(
        polygon_api_key=os.getenv("POLYGON_API_KEY", ""),
    )

    app.state.s3_client = S3Client(data_bucket_name=os.getenv("DATA_BUCKET_NAME", ""))

    DUCKDB_ACCESS_KEY = os.getenv("DUCKDB_ACCESS_KEY")  # noqa: N806
    DUCKDB_SECRET = os.getenv("DUCKDB_SECRET")  # noqa: N806

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

    if hasattr(app.state, "connection"):
        app.state.connection.close()


application = FastAPI(lifespan=lifespan)
Instrumentator().instrument(application).expose(application)

equity_bars_total_rows = Gauge(
    "equity_bars_total_rows",
    "Total number of rows in equity bars bucket",
)


@application.get("/health")
def get_health() -> Response:
    return Response(status_code=status.HTTP_200_OK)


@application.get("/metrics")
def get_metrics(request: Request) -> Response:
    try:
        count_query = f"""
            SELECT COUNT(*) as total_rows
            FROM read_parquet(
                'gs://{request.app.state.s3_client.data_bucket_name}/equity/bars/*/*/*/*', 
                HIVE_PARTITIONING=1
            )
        """  # noqa: S608

        result = request.app.state.connection.execute(count_query).fetchone()

        total_rows = result[0] if result else 0

        equity_bars_total_rows.set(total_rows)

        logger.info(f"Updated equity_bars_total_rows metric: {total_rows}")

        return Response(
            status_code=status.HTTP_200_OK,
            content=json.dumps({"total_rows": total_rows}),
            media_type="application/json",
        )

    except (
        duckdb.Error,
        IOException,
        ComputeError,
        GoogleAPIError,
    ) as e:
        logger.error(f"Error updating metrics: {e}")

        return Response(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            content=json.dumps({"error": "Failed to fetch metrics"}),
            media_type="application/json",
        )

    except requests.RequestException as e:  # TEMP
        logger.error(f"Request error while fetching metrics: {e}")
        return Response(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            content=json.dumps({"error": "Failed to fetch metrics"}),
            media_type="application/json",
        )


@application.get("/equity-bars")
def get_equity_bars(
    request: Request,
    start_date: date,
    end_date: date,
) -> Response:
    query = bars_query(
        bucket=request.app.state.s3_client.data_bucket_name,
        start_date=start_date,
        end_date=end_date,
    )

    try:
        data = request.app.state.connection.execute(query).arrow()

        if data.num_rows == 0:
            return Response(
                status_code=status.HTTP_404_NOT_FOUND,
                content=json.dumps({"error": "No data found for the given date range"}),
                media_type="application/json",
            )

        logger.info(f"Query returned rows count: {data.num_rows}")
        sink = pa.BufferOutputStream()
        with pa.ipc.RecordBatchStreamWriter(sink, data.schema) as writer:
            writer.write_table(data)

            filename = f"equity_bars_{start_date}_{end_date}.arrow"
            content_disposition = f"attachment; {filename=}"

        return Response(
            content=sink.getvalue().to_pybytes(),
            media_type="application/vnd.apache.arrow.file",
            headers={
                "Content-Disposition": content_disposition,
                "X-Row-Count": str(data.num_rows),
                "X-Start-Date": str(start_date),
                "X-End-Date": str(end_date),
            },
        )

    except (
        requests.RequestException,
        ComputeError,
        IOException,
        GoogleAPIError,
        pyarrow.lib.ArrowIOError,
    ) as e:
        logger.error(f"Error querying data: {e}")
        logger.error(traceback.format_exc())
        return Response(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            content=json.dumps({"error": f"Failed to query data: {e!s}"}),
            media_type="application/json",
        )


@application.post("/equity-bars/fetch", response_model=None)
def fetch_equity_bars(
    request: Request,
    summary_date: SummaryDate | None,
) -> CloudEvent:
    polygon_client = request.app.state.polygon_client
    daily_equity_bars_path = request.app.state.s3_client.daily_equity_bars_path

    if summary_date is None:
        summary_date = SummaryDate(
            date=datetime.now(tz=ZoneInfo("America/New_York")).date()
        )

    request_summary_date: str = summary_date.date.strftime("%Y-%m-%d")

    all_equity_bars_response = polygon_client.get_all_equity_bars(
        date=request_summary_date,
    )

    all_equity_bars = pl.DataFrame(all_equity_bars_response)
    count = len(all_equity_bars)
    if count > 0:
        try:
            all_equity_bars.with_columns(
                [
                    pl.from_epoch("t", time_unit="ms").alias("datetime"),
                    pl.from_epoch("t", time_unit="ms").dt.year().alias("year"),
                    pl.from_epoch("t", time_unit="ms").dt.month().alias("month"),
                    pl.from_epoch("t", time_unit="ms").dt.day().alias("day"),
                ],
            ).write_parquet(
                file=daily_equity_bars_path,
                partition_by=["year", "month", "day"],
            )
        except (
            requests.RequestException,
            ComputeError,
            IOException,
            GoogleAPIError,
            pyarrow.lib.ArrowIOError,
        ) as e:
            logger.error(f"Error writing parquet file: {e}")
            logger.error(traceback.format_exc())

            return CloudEvent(
                attributes={
                    "source": "datamanager",
                    "type": "application.datamanager.equity.bars.errored",
                },
                data={
                    "date": request_summary_date,
                    "error": str(e),
                },
            )

    return CloudEvent(
        attributes={
            "source": "datamanager",
            "type": "application.datamanager.equity.bars.created",
        },
        data={
            "date": request_summary_date,
            "count": count,
        },
    )


@application.delete("/equity-bars")
def delete_equity_bars(request: Request, summary_date: SummaryDate) -> Response:
    s3_client = request.app.state.s3_client
    year = summary_date.date.year
    month = summary_date.date.month
    day = summary_date.date.day
    prefix = f"equity/bars/{year=}/{month=}/{day=}"

    try:
        blobs = list(s3_client.list_objects(prefix=prefix))
    except exceptions.NotFound:
        return Response(
            status_code=status.HTTP_404_NOT_FOUND,
            content=json.dumps({"error": "No equity bars found"}),
            media_type="application/json",
        )
    if not blobs:
        return Response(
            status_code=status.HTTP_404_NOT_FOUND,
            content=json.dumps({"error": "No equity bars found for the given date"}),
            media_type="application/json",
        )

    logger.info(f"Deleting prefix: {prefix=}")
    s3_client.delete_objects(blobs)
    return Response(status_code=status.HTTP_204_NO_CONTENT)
