import os
import uuid
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any

import requests
from flytekit import task, workflow


@task
def fetch_data(start_date: datetime, end_date: datetime) -> List[Dict[str, Any]]:
    base_url = os.getenv("DATAMANAGER_BASE_URL", "http://localhost:8080")
    response = requests.get(
        f"{base_url}/equity-bars",
        params={"start_date": start_date.isoformat(), "end_date": end_date.isoformat()},
        timeout=10,
    )
    response.raise_for_status()
    return response.json().get("data", [])


@task
def train_dummy_model(data: List[Dict[str, Any]]) -> bytes:
    """Train a trivial model that stores the average close price."""
    close_prices = [row.get("close_price", 0.0) for row in data]
    mean_close = statistics.mean(close_prices) if close_prices else 0.0
    model = {"average_close_price": mean_close}
    return pickle.dumps(model)


@task
def store_model(model_bytes: bytes) -> str:
    """Store the serialized model in blob storage."""
    bucket_path = os.getenv("MODEL_BUCKET", "/tmp")
    filename = f"model-{uuid.uuid4().hex}.pkl"
    path = Path(bucket_path) / filename
    path.write_bytes(model_bytes)
    return str(path)


@workflow
def training_workflow(start_date: datetime, end_date: datetime) -> str:
    data = fetch_data(start_date=start_date, end_date=end_date)
    model_bytes = train_dummy_model(data=data)
    artifact_path = store_model(model_bytes=model_bytes)
    return artifact_path
