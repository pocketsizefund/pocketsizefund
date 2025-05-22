from behave import when
import requests


@when('I send a GET request to "{endpoint}"')
def step_impl(context, endpoint):
    """Send a GET request to the specified endpoint."""
    url = f"{context.api_url}{endpoint}"
    context.response = requests.get(url)
