import re
from datetime import date

import boto3
import duckdb
import polars as pl
from loguru import logger


class S3Client:
    def __init__(self, data_bucket_name: str) -> None:
        self.data_bucket_name = data_bucket_name
        self.daily_equity_bars_path = f"s3://{self.data_bucket_name}/equity/bars/"
        self.duckdb_connection = duckdb.connect()
        self._setup_s3_access()

    def _setup_s3_access(self) -> None:
        region = boto3.Session().region_name or "us-east-1"

        if not re.match(r"^[a-z0-9-]+$", region):
            message = f"Invalid S3 region format: {region}"
            raise ValueError(message)

        self.duckdb_connection.execute(f"""
            INSTALL httpfs;
            LOAD httpfs;
            SET s3_region='{region}';
        """)

    def close(self) -> None:
        if self.duckdb_connection:
            self.duckdb_connection.close()

    def __enter__(self):  # noqa: ANN204
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):  # noqa: ANN001, ANN204
        _ = exc_type, exc_val, exc_tb

        self.close()

    def write_equity_bars_data(self, data: pl.DataFrame) -> None:
        count = len(data)
        if count > 0:
            try:
                data.with_columns(
                    pl.from_epoch(pl.col("timestamp"), time_unit="ms").alias("dt")
                ).with_columns(
                    pl.col("dt").dt.year().alias("year"),
                    pl.col("dt").dt.month().alias("month"),
                    pl.col("dt").dt.day().alias("day"),
                ).drop("dt").write_parquet(
                    file=self.daily_equity_bars_path,
                    partition_by=["year", "month", "day"],
                )

            except Exception as e:
                logger.error(
                    f"Error writing equity bars data to bucket '{self.data_bucket_name}' "  # noqa: E501
                    f"at path '{self.daily_equity_bars_path}' with {count} rows: {e}"
                )
                raise

    def read_equity_bars_data(self, start_date: date, end_date: date) -> pl.DataFrame:
        path_pattern = f"s3://{self.data_bucket_name}/equity/bars/**/*.parquet"

        query = """ 
            SELECT *
            FROM read_parquet(
                ?, 
                HIVE_PARTITIONING=1
            )
            WHERE 
                (year > ? OR 
                (year = ? AND month > ?) OR 
                (year = ? AND month = ?
                AND day >= ?))
                AND
                (year < ? OR 
                (year = ? AND month < ?) OR 
                (year = ?
                AND month = ?
                AND day <= ?))
        """

        params = (
            path_pattern,
            start_date.year,
            start_date.year,
            start_date.month,
            start_date.year,
            start_date.month,
            start_date.day,
            end_date.year,
            end_date.year,
            end_date.month,
            end_date.year,
            end_date.month,
            end_date.day,
        )

        result = self.duckdb_connection.execute(query, params).fetchdf()

        return pl.from_pandas(result)
