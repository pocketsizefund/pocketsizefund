from behave import when
import requests


@when('I send a GET request to "{endpoint}"')
def step_impl(context, endpoint):
    url = f"{context.api_url}{endpoint}"
    context.response = requests.get(url)
