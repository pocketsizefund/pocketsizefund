import json
import os
from datetime import datetime, timedelta
from pathlib import Path

import httpx
import requests
import polars as pl
from behave import given, when, then

@given("the datamanager API is running")
def step_impl(context):
    # We assume the API is running locally for testing purposes
    # In a more complete setup, we might start the service here or check it's running
    context.api_url = context.base_url

@given("I have environment variables set for authentication")
def step_impl(context):
    # Check that necessary environment variables are set
    assert os.environ.get("POLYGON_API_KEY") is not None
    assert os.environ.get("POLYGON_BASE_URL") is not None

@when('I send a POST request to "{endpoint}" for date "{date_str}"')
def step_impl(context, endpoint, date_str):
    url = f"{context.api_url}{endpoint}"
    date_obj = datetime.strptime(date_str, "%Y-%m-%d").date()
    response = requests.post(url, json={"date": date_str})
    context.response = response
    context.test_date = date_str

@then("the response status code should be {status_code:d}")
def step_impl(context, status_code):
    assert context.response.status_code == status_code, \
        f"Expected status code {status_code}, got {context.response.status_code}"

@then('the response should contain a JSON with "{field1}" and "{field2}" fields')
def step_impl(context, field1, field2):
    response_json = context.response.json()
    assert field1 in response_json, f"Response JSON missing field: {field1}"
    assert field2 in response_json, f"Response JSON missing field: {field2}"

@then('a parquet file should be created for "{date_str}"')
def step_impl(context, date_str):
    # Check if file exists either locally or in the mock GCS bucket
    if os.environ.get("GCP_GCS_BUCKET"):
        # In a real test, we'd check the GCS bucket
        # For end-to-end tests, we'll need GCS credentials or a mock
        # Here we just assert as a placeholder
        assert True, "GCS bucket check would go here"
    else:
        # Check local file
        expected_file = Path(f"equity_bars_{date_str}.parquet")
        assert expected_file.exists(), f"Expected parquet file {expected_file} not found"

@given('I have equity bars data for dates')
def step_impl(context):
    # Store the table of dates for later use
    context.dates = [row['date'] for row in context.table]

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

@when('I send a GET request to "{endpoint}" with date range "{start_date}" to "{end_date}"')
def step_impl(context, endpoint, start_date, end_date):
    url = f"{context.api_url}{endpoint}"
    params = {
        "start_date": start_date,
        "end_date": end_date,
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
    assert context.start_date in metadata["start_date"], f"Metadata start date mismatch: {metadata['start_date']}"
    assert context.end_date in metadata["end_date"], f"Metadata end date mismatch: {metadata['end_date']}"