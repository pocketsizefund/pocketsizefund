from pathlib import Path
from os import path
import hashlib
from typing_extensions import Annotated
from typing import Optional, Union
from flytekit import (
    task,
    map_task,
    Secret,
    workflow,
    dynamic,
    current_context,
    Resources,
    ImageSpec,
)
from flytekit.experimental import eager
from polygon import RESTClient
import polars as pl
from datetime import date, datetime, timedelta, timezone
import pandera.polars as pa
from pandera.typing.polars import DataFrame
import s3fs
import pandas as pd


from workflows.schema.polygon import RawBars, Bars
from workflows.artifacts import RawBarsArtifact, BarsArtifact


price_model = ImageSpec(
    registry="docker.io",
    name="pocketsizefund/daily-bars",
    platform="linux/amd64",
    packages=[
        "polygon-api-client",
        "polars",
        "pandas",
        "pandera[polars]",
        "flytekitplugins-pandera",
        "s3fs",
    ],
)


@task(
    container_image=price_model,
    secret_requests=[Secret(key="POLYGON_API_KEY")],
)
def get_bars_by_date(date: date) -> pl.DataFrame:
    POLYGON_API_KEY = current_context().secrets.get(key="POLYGON_API_KEY")

    client = RESTClient(api_key=POLYGON_API_KEY)
    return pl.DataFrame(
        client.get_grouped_daily_aggs(
            date=date,
        )
    )


@task(
    container_image=price_model,
)
@pa.check_types
def process_bars(bars: pl.DataFrame) -> Optional[Annotated[DataFrame[Bars], BarsArtifact]]:
    if bars.shape[0] == 0:
        return None

    return pl.DataFrame(bars).select(
        [
            pl.col("ticker"),
            pl.col("open"),
            pl.col("close"),
            pl.col("high"),
            pl.col("low"),
            pl.col("volume"),
            pl.col("timestamp").cast(pl.Datetime("ms")).cast(pl.Date).alias("date"),
        ]
    )


@task(
    container_image=price_model,
)
def create_dates_from_range(start: date, end: date) -> list[date]:
    return [start + timedelta(days=i) for i in range((end - start).days + 1)]


@task(
    container_image=price_model,
    secret_requests=[
        Secret(key="AWS_ACCESS_KEY_ID"),
        Secret(key="AWS_SECRET_ACCESS_KEY"),
        Secret(key="DATA_BUCKET"),
    ],
)
def write_parquet(
    data: Optional[pl.DataFrame], date: date
) -> Optional[pl.DataFrame]:  # TODO: make this not optional, figure out better conditional flow
    # TODO: ensure file extension is parquet
    try:
        AWS_SECRET_ACCESS_KEY = current_context().secrets.get(key="AWS_SECRET_ACCESS_KEY")
        AWS_ACCESS_KEY_ID = current_context().secrets.get(key="AWS_ACCESS_KEY_ID")
        BUCKET = current_context().secrets.get(key="DATA_BUCKET")

        version = pd.util.hash_pandas_object(data.to_pandas()).values.tobytes()  # Get hash bytes
        version = hashlib.sha256(version).hexdigest()

        path = f"s3://{BUCKET}/data/{date.year}/{date.month:02d}/{date.day:02d}/{version}.parquet"

        fs = s3fs.S3FileSystem(secret=AWS_SECRET_ACCESS_KEY, key=AWS_ACCESS_KEY_ID)

        with fs.open(path, "wb") as f:
            data.write_parquet(f, compression="zstd")

        return data
    except:
        return None


# NOTE: Optional returns None for days without bars, i.e., days when market closed
@workflow
def load_daily_bars(date: date) -> Optional[pl.DataFrame]:
    bars = process_bars(get_bars_by_date(date))

    return write_parquet(bars, date)


@workflow
def load_yesterdays_bars() -> Optional[pl.DataFrame]:
    yesterday = (datetime.now(timezone.utc) - timedelta(days=1)).date()

    return load_daily_bars(yesterday)


def backfill_daily_bars() -> None:
    start = datetime(year=2022, month=1, day=1)
    end = datetime.now()
    dates = [(start + timedelta(days=i)).date() for i in range((end - start).days + 1)]

    for date in dates:
        load_daily_bars(date)

    return None
