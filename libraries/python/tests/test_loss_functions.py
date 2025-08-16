import numpy as np
import pytest
from internal.loss_functions import quantile_loss
from numpy.random import PCG64, Generator
from tinygrad.tensor import Tensor

rng = Generator(PCG64())


def test_quantile_loss_basic() -> None:
    predictions = Tensor([[[1.0, 1.2, 1.5]], [[2.0, 2.1, 2.3]], [[3.0, 2.8, 2.9]]])
    targets = Tensor([[2.0], [2.5], [1.8]])
    quantiles = [0.25, 0.5, 0.75]

    loss = quantile_loss(predictions, targets, quantiles)

    assert isinstance(loss, Tensor)
    assert len(loss.shape) == 0 or loss.shape in [(), (0,), (1,)]


def test_quantile_loss_multiple_samples() -> None:
    predictions = Tensor([[[1.0, 1.1, 1.3]], [[2.0, 2.2, 2.4]]])
    targets = Tensor([[2.5], [5.5]])
    quantiles = [0.25, 0.5, 0.75]

    loss: Tensor = quantile_loss(predictions, targets, quantiles)

    assert isinstance(loss, Tensor)
    assert len(loss.shape) == 0 or loss.shape in [(), (0,), (1,)]


def test_quantile_loss_perfect_prediction() -> None:
    predictions = Tensor([[[2.0, 2.0, 2.0]]])
    targets = Tensor([[2.0]])
    quantiles = [0.25, 0.5, 0.75]

    loss = quantile_loss(predictions, targets, quantiles)

    assert loss.numpy() >= 0.0


def test_quantile_loss_different_quantiles() -> None:
    predictions = Tensor([[[3.0, 3.1, 3.0, 2.9, 3.0]]])
    targets = Tensor([[3.0]])
    quantiles = [0.1, 0.25, 0.5, 0.75, 0.9]

    loss = quantile_loss(predictions, targets, quantiles)

    assert isinstance(loss, Tensor)
    assert loss.numpy() >= 0.0


def test_quantile_loss_shapes() -> None:
    for batch_size in [1, 2, 4, 8]:
        predictions = Tensor(rng.standard_normal((batch_size, 1, 3)).astype(np.float32))
        targets = Tensor(rng.standard_normal((batch_size, 1)).astype(np.float32))
        quantiles = [0.25, 0.5, 0.75]

        loss = quantile_loss(predictions, targets, quantiles)
        assert isinstance(loss, Tensor)


def test_quantile_loss_invalid_quantiles() -> None:
    predictions = Tensor([[[1.0, 1.2, 1.3]]])
    targets = Tensor([[2.0]])
    quantiles = [0.25, 1.5, 0.75]  # Invalid quantile > 1

    with pytest.raises(ValueError, match="All quantiles must be between 0 and 1"):
        quantile_loss(predictions, targets, quantiles)
