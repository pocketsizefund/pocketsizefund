# api_check.py

import os
from functools import wraps
from flask import abort, Flask
from loguru import logger

REQUIRED_API_KEYS = {
    "DARQUBE_API_KEY",
    "ALPACA_API_KEY",
    "ALPACA_API_SECRET",
    "ALPHA_VANTAGE_API_KEY",
    "EDGAR_USER_AGENT",
    "MODEL_FILE_NAME"
}

def api_key_required(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        for key in REQUIRED_API_KEYS:
            if not os.getenv(key):
                logger.error(f"Missing API key: {key}")
                abort(401, description=f"Missing API key: {key}")
        return f(*args, **kwargs)
    return decorated_function