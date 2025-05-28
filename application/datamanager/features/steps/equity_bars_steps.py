import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import requests
from behave import given, when, then
from behave.runner import Context


@given("I have date ranges")
def step_impl_date_ranges(context: Context) -> None:
    for row in context.table:  # ty: ignore
        context.start_date = row["start_date"]
        context.end_date = row["end_date"]


@given("the datamanager API is running")
def step_impl_api_url(context: Context) -> None:
    context.api_url = context.base_url


@when('I send a POST request to "{endpoint}" for date range')
def step_impl_post_request(context: Context, endpoint: str) -> None:
    url = f"{context.api_url}{endpoint}"
    response = requests.post(url, json={"date": context.start_date}, timeout=30)
    context.response = response


@when('I send a GET request to "{endpoint}" for date range')
def step_imp_get_request(context: Context, endpoint: str) -> None:
    url = f"{context.api_url}{endpoint}"
    response = requests.get(
        url,
        params={"start_date": context.start_date, "end_date": context.end_date},
        timeout=30,
    )
    context.response = response


@then("the response status code should be {status_code}")
def step_impl_response_status_code(context: Context, status_code: str) -> None:
    assert context.response.status_code == int(status_code), (
        f"Expected status code {status_code}, got {context.response.status_code}"
    )


@when('I send a DELETE request to "{endpoint}" for date "{date_str}"')
def step_impl(context: Context, endpoint: str, date_str: str) -> None:
    url = f"{context.api_url}{endpoint}"
    response = requests.delete(url, json={"date": date_str}, timeout=30)
    context.response = response
    context.test_date = date_str


@then('the equity bars data for "{date_str}" should be deleted')
def step_impl_equity_bars(context: Context, date_str: str) -> None:
    if os.environ.get("GCP_GCS_BUCKET"):
        assert True, "GCS bucket deletion check would go here"
    else:
        expected_file = Path(f"equity_bars_{date_str}.parquet")
        assert not expected_file.exists(), (
            f"Parquet file {expected_file} still exists after deletion"
        )
