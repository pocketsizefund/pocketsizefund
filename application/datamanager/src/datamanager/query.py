from datetime import timedelta
from datetime import timedelta
from polars import DataFrame
import duckdb
from pathlib import Path

from .models import DateRange


async def list_file_paths(*, bucket: str, date_range: DateRange) -> list[Path]:
    filepaths = []
    current = date_range.start
    while current <= date_range.end:
        filepaths.append(
            f"gs://{bucket}/equity/bars/{current.strftime('%Y-%m-%d')}/data.parquet"
        )
        current += timedelta(days=1)

    return filepaths


async def query_bars(*, filepaths: list[Path]) -> DataFrame:
    query = f"""
    SELECT * FROM read_parquet({filepaths})
    """
    return duckdb.sql(query).pl()
