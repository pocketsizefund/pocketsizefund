from unittest.mock import patch
import numpy as np
import polars as pl
import pytest
import tensorflow as tf
from keras import models
from pipelines.lstm import shape_timeseries_dataset, create_model, save_model, evaluate_model
from pipelines.types import TimeWindow


@pytest.mark.parametrize("shuffle", [True, False])
def test_shape_timeseries_dataset(shuffle):
    data = pl.DataFrame({"feature1": range(10), "feature2": range(10, 20)})
    features = ["feature1", "feature2"]
    close_price_index = 0
    window = TimeWindow(input=3, output=2)
    stride = 1
    batch_size = 2

    dataset = shape_timeseries_dataset(
        data=data,
        features=features,
        close_price_index=close_price_index,
        window=window,
        stride=stride,
        shuffle=shuffle,
        batch_size=batch_size,
    )

    assert isinstance(dataset, tf.data.Dataset)
    for inputs, labels in dataset.take(1):
        assert inputs.shape == (batch_size, window.input, len(features))
        assert labels.shape == (batch_size, window.output, 1)  # Only one feature as label


def test_create_model():
    label_count = 1
    window = TimeWindow(input=3, output=2)
    model = create_model(label_count=label_count, window=window)

    # Check model configuration
    assert isinstance(model, models.Sequential)
    assert len(model.layers) == 3  # LSTM, Dense, Reshape
