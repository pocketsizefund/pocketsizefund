import os
import tempfile
import uuid
from datetime import datetime
from pathlib import Path
from typing import Any, cast

import polars as pl
import pyarrow as pa
import requests
from flytekit import task, workflow
from loguru import logger
from tinygrad.nn.state import get_state_dict, safe_save

from application.predictionengine.src.predictionengine.dataset import DataSet
from application.predictionengine.src.predictionengine.miniature_temporal_fusion_transformer import (  # noqa: E501
    MiniatureTemporalFusionTransformer,
)


@task
def fetch_data(
    start_date: datetime,
    end_date: datetime,
) -> list[dict[str, Any]]:
    base_url = os.getenv("DATAMANAGER_BASE_URL", "http://localhost:8080")
    response = requests.get(
        f"{base_url}/equity-bars",
        params={
            "start_date": start_date.date().isoformat(),
            "end_date": end_date.date().isoformat(),
        },
        timeout=30,
    )
    response.raise_for_status()

    buffer = pa.py_buffer(response.content)
    reader = pa.ipc.RecordBatchStreamReader(buffer)
    table = reader.read_all()

    data = pl.DataFrame(pl.from_arrow(table))

    data = data.with_columns(
        [
            pl.col("t").cast(pl.Datetime).alias("timestamp"),
            pl.col("o").alias("open_price"),
            pl.col("h").alias("high_price"),
            pl.col("l").alias("low_price"),
            pl.col("c").alias("close_price"),
            pl.col("v").alias("volume"),
            pl.col("vw").alias("volume_weighted_average_price"),
            pl.col("T").alias("ticker"),
        ]
    ).select(
        [
            "timestamp",
            "open_price",
            "high_price",
            "low_price",
            "close_price",
            "volume",
            "volume_weighted_average_price",
            "ticker",
        ]
    )

    return data.to_dicts()


@task
def train_model(
    data: list[dict[str, Any]],
    epochs: int = 100,
) -> bytes:
    if not data:
        msg = "No data provided for training"
        raise ValueError(msg)

    training_data = pl.DataFrame(data)

    dataset = DataSet(
        batch_size=32,
        sequence_length=30,
        sample_count=len(training_data),
    )
    dataset.load_data(training_data)
    preprocessors = dataset.get_preprocessors()

    model = MiniatureTemporalFusionTransformer(
        input_size=6,
        hidden_size=128,
        output_size=3,
        layer_count=2,
        ticker_count=len(training_data["ticker"].unique()),
        embedding_size=32,
        attention_head_count=4,
        means_by_ticker=preprocessors["means_by_ticker"],
        standard_deviations_by_ticker=preprocessors["standard_deviations_by_ticker"],
        ticker_encoder=preprocessors["ticker_encoder"],
        dropout_rate=0.1,
    )

    losses = model.train(dataset, epochs, learning_rate=0.001)

    for epoch, loss in enumerate(losses):
        if epoch % 10 == 0:
            logger.info(f"Epoch {epoch}, Loss: {loss}")

    with tempfile.NamedTemporaryFile(
        suffix=".safetensor",
        delete=False,
    ) as temporary_file:
        safe_save(get_state_dict(model), temporary_file.name)
        temporary_file.seek(0)
        model_bytes = temporary_file.read()

    return model_bytes  # noqa: RET504


@task
def store_model(model_bytes: bytes) -> str:
    bucket_path = os.getenv("MODEL_BUCKET", "/tmp")  # noqa: S108
    filename = f"miniature_temporal_fusion_transformer-{uuid.uuid4().hex}.safetensor"
    path = Path(bucket_path) / filename
    path.write_bytes(model_bytes)
    return str(path)


@workflow
def training_workflow(
    start_date: datetime,
    end_date: datetime,
    epochs: int = 100,
) -> str:
    data = fetch_data(start_date=start_date, end_date=end_date)
    model_bytes = train_model(data=cast("list[dict[str, Any]]", data), epochs=epochs)
    artifact_path = store_model(model_bytes=cast("bytes", model_bytes))
    return cast("str", artifact_path)
