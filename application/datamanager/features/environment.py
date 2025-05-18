import os
import json
import sys
from pathlib import Path
from behave import fixture

# Add the project root to the Python path
sys.path.insert(0, str(Path(__file__).parent.parent))

def before_all(context):
    """Set up test environment."""
    # Set base URL from environment or use default for docker
    context.base_url = os.environ.get("BASE_URL", "http://datamanager:8000")

    # These should be set in the container but we'll provide fallbacks
    if not os.environ.get("POLYGON_API_KEY"):
        os.environ["POLYGON_API_KEY"] = "dummy-test-key"

    if not os.environ.get("POLYGON_BASE_URL"):
        os.environ["POLYGON_BASE_URL"] = "http://polygon-mock:8080"

    if not os.environ.get("GCP_GCS_BUCKET"):
        os.environ["GCP_GCS_BUCKET"] = "test-bucket"

    print(f"Using base URL: {context.base_url}")
    print(f"Using Polygon API URL: {os.environ.get('POLYGON_BASE_URL')}")