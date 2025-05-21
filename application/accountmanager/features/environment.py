import os


def before_all(context):
    context.base_url = os.environ.get("BASE_URL", "http://accountmanager:8080")
