import tempfile
from pathlib import Path

import polars as pl
from prefect import task
from prefect_aws.s3 import S3Bucket

from pipelines.types import Bucket


@task(retries=3)
def load_dataframe(bucket: Bucket) -> pl.DataFrame:
    s3_bucket_block = S3Bucket.load(bucket.block)
    data = s3_bucket_block.read_path(path=f"{bucket.prefix}/{bucket.key}")
    with tempfile.NamedTemporaryFile("w+", encoding="utf-8", suffix=".csv") as file:
        s3_bucket_block.download_object_to_path(
            f"{bucket.prefix}/{bucket.key}", file.name
        )

        data = pl.read_csv(file.name)

    return data