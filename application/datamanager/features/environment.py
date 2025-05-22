import os


def before_all(context):
    """Set up test environment."""
    context.base_url = os.environ.get("BASE_URL", "http://datamanager:8000")
