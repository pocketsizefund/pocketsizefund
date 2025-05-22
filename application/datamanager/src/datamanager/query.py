from datetime import timedelta, date
from polars import DataFrame
import duckdb
from pathlib import Path


async def list_file_paths(
    *, bucket: str, start_date: date, end_date: date
) -> list[Path]:
    filepaths = []
    current = start_date
    while current <= end_date:
        filepaths.append(
            f"gs://{bucket}/equity/bars/{current.strftime('%Y-%m-%d')}/data.parquet"
        )
        current += timedelta(days=1)

    return filepaths


async def query_bars(*, filepaths: list[Path]) -> DataFrame:
    query = f"""
    SELECT * FROM read_parquet({filepaths})
    """
    try:
        return duckdb.sql(query).pl()
    except duckdb.duckdb.HTTPException as e:
    return 
