import requests
from behave import when
from behave.runner import Context


@when('I send a GET request to "{endpoint}"')
def step_impl(context: Context, endpoint: str) -> None:
    url = f"{context.api_url}{endpoint}"
    context.response = requests.get(url, timeout=30)
