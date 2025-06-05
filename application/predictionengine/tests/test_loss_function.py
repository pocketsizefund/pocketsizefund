import numpy as np
import pytest
from tinygrad.tensor import Tensor

from application.predictionengine.src.predictionengine.loss_function import (
    quantile_loss,
)


def test_quantile_loss_basic() -> None:
    predictions = Tensor([[1.0], [2.0], [3.0]])
    targets = Tensor([[2.0], [2.5], [1.8]])
    quantiles = (0.25, 0.5, 0.75)

    loss = quantile_loss(predictions, targets, quantiles)

    assert isinstance(loss, Tensor)
    assert loss.shape == () or loss.shape == (1,)  # noqa: PLR1714


def test_quantile_loss_multiple_samples() -> None:
    predictions = Tensor([[1.0], [2.0]])
    targets = Tensor([[2.5], [5.5]])
    quantiles = (0.25, 0.5, 0.75)

    loss = quantile_loss(predictions, targets, quantiles)

    assert isinstance(loss, Tensor)
    assert loss.shape == () or loss.shape == (1,)  # noqa: PLR1714


def test_quantile_loss_perfect_prediction() -> None:
    predictions = Tensor([[2.0]])
    targets = Tensor([[2.0]])
    quantiles = (0.25, 0.5, 0.75)

    loss = quantile_loss(predictions, targets, quantiles)

    assert loss.numpy() >= 0.0


def test_quantile_loss_different_quantiles() -> None:
    predictions = Tensor([[3.0]])
    targets = Tensor([[3.0]])
    quantiles = (0.1, 0.25, 0.5, 0.75, 0.9)

    loss = quantile_loss(predictions, targets, quantiles)

    assert isinstance(loss, Tensor)
    assert loss.numpy() >= 0.0


def test_quantile_loss_shapes() -> None:
    for batch_size in [1, 2, 4, 8]:
        default_range = np.random.default_rng()
        predictions = Tensor(
            default_range.standard_normal((batch_size, 1)).astype(np.float32)
        )
        targets = Tensor(
            default_range.standard_normal((batch_size, 1)).astype(np.float32)
        )
        quantiles = (0.25, 0.5, 0.75)

        loss = quantile_loss(predictions, targets, quantiles)
        assert isinstance(loss, Tensor)


def test_quantile_loss_shape_mismatch() -> None:
    predictions = Tensor([[1.0, 2.0, 3.0]])
    targets = Tensor([[2.0]])
    quantiles = (0.25, 0.5, 0.75)

    with pytest.raises(ValueError, match="Shape mismatch"):
        quantile_loss(predictions, targets, quantiles)


def test_quantile_loss_invalid_quantiles() -> None:
    predictions = Tensor([[1.0]])
    targets = Tensor([[2.0]])
    quantiles = (0.25, 1.5, 0.75)  # Invalid quantile > 1

    with pytest.raises(ValueError, match="All quantiles must be between 0 and 1"):
        quantile_loss(predictions, targets, quantiles)
