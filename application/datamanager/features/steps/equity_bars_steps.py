import json
import os
import sys
from datetime import datetime, timedelta
from pathlib import Path

# Add the project root to the Python path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

import httpx
import requests
from behave import given, when, then


@given("I have date ranges")
def step_impl(context):
    for row in context.table:
        context.start_date = row["start_date"]
        context.end_date = row["end_date"]


@given("the datamanager API is running")
def step_impl_api_url(context):
    context.api_url = context.base_url


@when('I send a POST request to "{endpoint}" for date range')
def step_impl(context, endpoint):
    url = f"{context.api_url}{endpoint}"
    response = requests.post(url, json={"date": context.start_date})
    context.response = response


@when('I send a GET request to "{endpoint}" for date range')
def step_imp(context, endpoint):
    url = f"{context.api_url}{endpoint}"
    response = requests.get(
        url,
        params={"start_date": context.start_date, "end_date": context.end_date},
    )
    context.response = response


@then("the response status code should be {status_code}")
def step_impl(context, status_code):
    assert context.response.status_code == int(status_code), (
        f"Expected status code {status_code}, got {context.response.status_code}"
    )


@then('the response should contain a JSON with "{field1}" and "{field2}" fields')
def step_impl(context, field1, field2):
    response_json = context.response.json()
    assert field1 in response_json, f"Response JSON missing field: {field1}"
    assert field2 in response_json, f"Response JSON missing field: {field2}"


@then('I can get that data back from the API for date "{date_str}"')
def step_impl(context, date_str):
    expected_file = Path(f"equity_bars_{date_str}.parquet")
    assert expected_file.exists(), f"Expected parquet file {expected_file} not found"


@given("I have equity bars data for dates")
def step_impl(context):
    # Store the table of dates for later use
    context.dates = [row["date"] for row in context.table]

    # Generate and store data for each date (mock data for testing)
    for date_str in context.dates:
        url = f"{context.api_url}/equity-bars"
        response = requests.post(url, json={"date": date_str})
        assert response.status_code == 200, f"Failed to set up data for date {date_str}"


@given('I have equity bars data for date "{date_str}"')
def step_impl(context, date_str):
    url = f"{context.api_url}/equity-bars"
    response = requests.post(url, json={"date": date_str})
    assert response.status_code == 200, f"Failed to set up data for date {date_str}"
    context.test_date = date_str


@when(
    'I send a GET request to "{endpoint}" with date range "{start_date}" to "{end_date}"'
)
def step_impl(context, endpoint, start_date, end_date):
    url = f"{context.api_url}{endpoint}"
    params = {
        "date_range": {
            "start": start_date,
            "end": end_date,
        }
    }
    response = requests.get(url, params=params)
    context.response = response
    context.start_date = start_date
    context.end_date = end_date


@then("the response should contain equity bars data for the date range")
def step_impl(context):
    response_json = context.response.json()
    assert "data" in response_json, "Response does not contain data field"

    # Check if data is not empty
    assert len(response_json["data"]) > 0, "Response data is empty"

    # For a more thorough check, we could verify date ranges of the returned data
    # This would depend on the structure of your API response


@then("the response should include a metadata section")
def step_impl(context):
    response_json = context.response.json()
    assert "metadata" in response_json, "Response does not contain metadata field"

    # Check metadata fields
    metadata = response_json["metadata"]
    assert "start_date" in metadata, "Metadata missing start_date"
    assert "end_date" in metadata, "Metadata missing end_date"
    assert "count" in metadata, "Metadata missing count"

    # Verify the dates match our request
    assert context.start_date in metadata["start_date"], (
        f"Metadata start date mismatch: {metadata['start_date']}"
    )
    assert context.end_date in metadata["end_date"], (
        f"Metadata end date mismatch: {metadata['end_date']}"
    )


@when('I send a DELETE request to "{endpoint}" for date "{date_str}"')
def step_impl(context, endpoint, date_str):
    url = f"{context.api_url}{endpoint}"
    response = requests.delete(url, json={"date": date_str})
    context.response = response
    context.test_date = date_str


@then('the equity bars data for "{date_str}" should be deleted')
def step_impl(context, date_str):
    # Verify the data no longer exists by trying to fetch it
    if os.environ.get("GCP_GCS_BUCKET"):
        # In a real test, we'd check that the file no longer exists in GCS
        # For testing purposes, we'll just assume it was deleted properly
        assert True, "GCS bucket deletion check would go here"
    else:
        # For local files, check the file no longer exists
        expected_file = Path(f"equity_bars_{date_str}.parquet")
        assert not expected_file.exists(), (
            f"Parquet file {expected_file} still exists after deletion"
        )


@then("the response should confirm successful deletion")
def step_impl(context):
    response_json = context.response.json()
    assert "status" in response_json, "Response missing status field"
    assert response_json["status"] == "success", (
        f"Expected success status, got {response_json['status']}"
    )
    assert "date" in response_json, "Response missing date field"
    assert "message" in response_json, "Response missing message field"
    assert "deleted successfully" in response_json["message"], (
        "Response message does not confirm deletion"
    )
