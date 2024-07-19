"""All project-level configuration."""

import datetime
import os
from functools import wraps
from typing import Any, Callable

from flask import abort
from loguru import logger

ENVIRONMENT_DEVELOPMENT = "development"
TIMEZONE = datetime.timezone.utc  # noqa: UP017

REQUIRED_API_KEYS = {
    "DARQUBE_API_KEY",
    "ALPACA_API_KEY",
    "ALPACA_API_SECRET",
    "ALPHA_VANTAGE_API_KEY",
    "EDGAR_USER_AGENT",
    "MODEL_FILE_NAME",
}


def api_key_required(f: Callable) -> Callable:
    """Decorate flask endpoints with required API keys check."""

    @wraps(f)
    def decorated_function(*args: Any, **kwargs: Any) -> Callable:
        for key in REQUIRED_API_KEYS:
            if not os.getenv(key):
                logger.error(f"Missing API key: {key}")
                abort(401, description=f"Missing API key: {key}")
        return f(*args, **kwargs)

    return decorated_function
