from behave import given, when, then
import requests


@given("the accountmanager API is running")
def step_impl_api_url(context):
    context.api_url = context.base_url


@when('I send a GET request to "{endpoint}"')
def step_impl(context, endpoint):
    """Send a GET request to the specified endpoint."""
    url = f"{context.api_url}{endpoint}"
    context.response = requests.get(url)


@then("the response status code should be {status_code}")
def step_impl(context, status_code):
    assert context.response.status_code == int(status_code), (
        f"Expected status code {status_code}, got {context.response.status_code}"
    )
