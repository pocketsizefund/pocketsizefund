from union import workflow, task, Secret, ImageSpec, current_context
import s3fs
import polars as pl
from pandera.typing.polars import DataFrame

from workflows.schema import RawBars

s3_image = ImageSpec(
    name="pocketsizefund/s3",
    platform="linux/amd64",
    packages=[
        "polars",
        "pandas",
        "pandera[polars]",
        "flytekitplugins-pandera",
        "s3fs",
    ],
)


@task(
    container_image=s3_image,
    secret_requests=[
        Secret(key="AWS_ACCESS_KEY_ID"),
        Secret(key="AWS_SECRET_ACCESS_KEY"),
        Secret(key="DATA_BUCKET"),
    ],
)
def list_available_data(*, prefix: str = "", file_extension: str = "parquet") -> list[str]:
    AWS_SECRET_ACCESS_KEY = current_context().secrets.get(key="AWS_SECRET_ACCESS_KEY")
    AWS_ACCESS_KEY_ID = current_context().secrets.get(key="AWS_ACCESS_KEY_ID")
    BUCKET = current_context().secrets.get(key="DATA_BUCKET")

    fs = s3fs.S3FileSystem(secret=AWS_SECRET_ACCESS_KEY, key=AWS_ACCESS_KEY_ID)
    return [
        f"s3://{path}"
        for path in fs.find(f"{BUCKET}/{prefix}", maxdepth=None)
        if path.endswith(file_extension)
    ]


@task(
    container_image=s3_image,
    secret_requests=[
        Secret(key="AWS_ACCESS_KEY_ID"),
        Secret(key="AWS_SECRET_ACCESS_KEY"),
        Secret(key="DATA_BUCKET"),
    ],
)
def load_bars(paths: list[str]) -> DataFrame[RawBars]:
    AWS_SECRET_ACCESS_KEY = current_context().secrets.get(key="AWS_SECRET_ACCESS_KEY")
    AWS_ACCESS_KEY_ID = current_context().secrets.get(key="AWS_ACCESS_KEY_ID")

    return pl.scan_parquet(
        paths,
        storage_options={
            "aws_access_key_id": AWS_ACCESS_KEY_ID,
            "aws_secret_access_key": AWS_SECRET_ACCESS_KEY,
            "aws_region": "us-east-1",
        },
    )


@workflow
def shape_bars_data() -> pl.LazyFrame:
    paths = list_available_data()
    return load_bars(paths)
