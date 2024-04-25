from pathlib import Path

import polars as pl
from prefect import task


@task(retries=3)
def load_dataframe(path: Path) -> pl.DataFrame:
    return pl.read_csv(path)
