import os

from behave.runner import Context


def before_all(context: Context) -> None:
    context.base_url = os.environ.get("BASE_URL", "http://datamanager:8080")
