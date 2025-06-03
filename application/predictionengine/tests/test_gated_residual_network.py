from tinygrad.tensor import Tensor
import numpy as np
from application.predictionengine.src.predictionengine.gated_residual_network import (
    GatedResidualNetwork,
)


def test_gated_residual_network_initialization() -> None:
    input_size = 64
    hidden_size = 128
    output_size = 32

    grn = GatedResidualNetwork(
        input_size=input_size,
        hidden_size=hidden_size,
        output_size=output_size,
    )

    assert grn.dense_input.weight.shape == (hidden_size, input_size)

    assert grn.dense_output.weight.shape == (output_size, hidden_size)

    assert grn.gate.weight.shape == (output_size, hidden_size)


def test_gated_residual_network_forward() -> None:
    grn = GatedResidualNetwork(input_size=32, hidden_size=64, output_size=32)

    input_tensor = Tensor(np.random.randn(8, 32))
    output = grn.forward(input_tensor)

    assert output.shape == (8, 32)


def test_gated_residual_network_different_sizes() -> None:
    grn = GatedResidualNetwork(input_size=16, hidden_size=32, output_size=8)

    input_tensor = Tensor(np.random.randn(4, 16))
    output = grn.forward(input_tensor)

    assert output.shape == (4, 8)


def test_gated_residual_network_single_sample() -> None:
    grn = GatedResidualNetwork(input_size=10, hidden_size=20, output_size=10)

    input_tensor = Tensor(np.random.randn(1, 10))
    output = grn.forward(input_tensor)

    assert output.shape == (1, 10)


def test_gated_residual_network_consistency() -> None:
    grn = GatedResidualNetwork(input_size=16, hidden_size=32, output_size=16)

    input_tensor = Tensor(np.random.randn(2, 16))

    output1 = grn.forward(input_tensor)
    output2 = grn.forward(input_tensor)

    assert output1.shape == output2.shape
    assert np.allclose(output1.numpy(), output2.numpy())
