"""Step definitions for bucket management."""

import os
import duckdb
import gcsfs
from behave import given, when, then


def _bucket_path(start_date: str, end_date: str) -> str:
    bucket = os.environ.get("DATA_BUCKET", "test-bucket")
    return f"gs://{bucket}/equity/bars/{start_date}_{end_date}/data.parquet"


@given('the bucket object for "{date_str}" does not exist')
def step_impl(context, date_str):
    context.bucket_path = _bucket_path(date_str, context.end_date)


@then('the bucket object for "{date_str}" should exist')
def step_impl(context, date_str):
    fs = gcsfs.GCSFileSystem(token=os.environ.get("GOOGLE_APPLICATION_CREDENTIALS"))
    assert fs.exists(context.bucket_path), f"{context.bucket_path} does not exist"


@then('the bucket object for "{date_str}" should not exist')
def step_impl(context, date_str):
    fs = gcsfs.GCSFileSystem(token=os.environ.get("GOOGLE_APPLICATION_CREDENTIALS"))
    assert not fs.exists(context.bucket_path), f"{context.bucket_path} still exists"


@when('I read the bucket object for "{date_str}" with DuckDB')
def step_impl(context, date_str):
    con = duckdb.connect()
    query = (
        f"SELECT * FROM read_parquet('{context.bucket_path}') "
        f"WHERE date >= '{context.start_date}' AND date <= '{context.end_date}'"
    )
    context.duckdb_df = con.sql(query).df()


@then('the result should have rows')
def step_impl(context):
    assert getattr(context, "duckdb_df", None) is not None
    assert len(context.duckdb_df) > 0
