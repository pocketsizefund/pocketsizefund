import os
import threading
from http.server import HTTPServer

from behave import fixture

from datamanager.src.datamanager.main import application
from tests.test_fetch import Handler


@fixture
def server_fixture(context):
    """Set up test environment."""
    # Start mock Polygon API server
    context.server = HTTPServer(("localhost", 0), Handler)
    context.thread = threading.Thread(target=context.server.serve_forever)
    context.thread.daemon = True
    context.thread.start()

    # Set environment variables
    os.environ["POLYGON_API_KEY"] = "dummy"
    os.environ["POLYGON_BASE_URL"] = (
        f"http://{context.server.server_address[0]}:{context.server.server_address[1]}"
    )

    # If using test bucket, set environment variable
    os.environ["GCP_GCS_BUCKET"] = "test-bucket"

    # Set up FastAPI application
    context.app = application

    yield context

    # Clean up
    context.server.shutdown()
    context.thread.join()


def before_all(context):
    # Use the fixture
    server_fixture(context)

    # Base URL for API requests
    context.base_url = "http://localhost:8000"
