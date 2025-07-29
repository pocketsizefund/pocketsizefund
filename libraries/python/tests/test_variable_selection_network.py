import numpy as np
from internal.variable_selection_network import VariableSelectionNetwork
from numpy.random import PCG64, Generator
from tinygrad.tensor import Tensor

rng = Generator(PCG64())


def test_variable_selection_network_initialization() -> None:
    input_dimension = 32
    hidden_size = 64

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    assert hasattr(vsn, "input_layer")
    assert hasattr(vsn, "output_layer")
    assert vsn.input_layer.weight.shape[0] == hidden_size
    assert vsn.input_layer.weight.shape[1] == input_dimension
    assert vsn.output_layer.weight.shape[0] == input_dimension
    assert vsn.output_layer.weight.shape[1] == hidden_size


def test_variable_selection_network_forward_basic() -> None:
    input_dimension = 16
    hidden_size = 32
    batch_size = 4

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    input_tensor = Tensor(
        rng.standard_normal((batch_size, input_dimension)).astype(np.float32)
    )
    output = vsn.forward(input_tensor)

    assert output.shape == (batch_size, input_dimension)


def test_variable_selection_network_output_range() -> None:
    input_dimension = 8
    hidden_size = 16

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    input_tensor = Tensor(rng.standard_normal((2, input_dimension)).astype(np.float32))
    output = vsn.forward(input_tensor)

    output_numpy = output.numpy()
    assert np.all(output_numpy >= 0.0)
    assert np.all(output_numpy <= 1.0)


def test_variable_selection_network_different_batch_sizes() -> None:
    input_dimension = 12
    hidden_size = 24

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    for batch_size in [1, 2, 8, 16]:
        input_tensor = Tensor(
            rng.standard_normal((batch_size, input_dimension)).astype(np.float32)
        )
        output = vsn.forward(input_tensor)

        assert output.shape == (batch_size, input_dimension)


def test_variable_selection_network_different_dimensions() -> None:
    test_configs = [
        (4, 8),
        (10, 20),
        (32, 64),
        (64, 32),
        (100, 50),
    ]

    for input_dimension, hidden_size in test_configs:
        vsn = VariableSelectionNetwork(
            input_dimension=input_dimension,
            hidden_size=hidden_size,
        )

        input_tensor = Tensor(
            rng.standard_normal((2, input_dimension)).astype(np.float32)
        )
        output = vsn.forward(input_tensor)

        assert output.shape == (2, input_dimension)


def test_variable_selection_network_zero_input() -> None:
    input_dimension = 16
    hidden_size = 32

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    input_tensor = Tensor(np.zeros((3, input_dimension)).astype(np.float32))
    output = vsn.forward(input_tensor)

    assert output.shape == (3, input_dimension)
    output_numpy = output.numpy()
    assert np.all(output_numpy >= 0.0)
    assert np.all(output_numpy <= 1.0)


def test_variable_selection_network_positive_input() -> None:
    input_dimension = 20
    hidden_size = 40

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    input_tensor = Tensor(
        np.abs(rng.standard_normal((4, input_dimension))).astype(np.float32)
    )
    output = vsn.forward(input_tensor)

    assert output.shape == (4, input_dimension)
    output_numpy = output.numpy()
    assert np.all(output_numpy >= 0.0)
    assert np.all(output_numpy <= 1.0)


def test_variable_selection_network_negative_input() -> None:
    input_dimension = 24
    hidden_size = 48

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    input_tensor = Tensor(
        -np.abs(rng.standard_normal((3, input_dimension))).astype(np.float32)
    )
    output = vsn.forward(input_tensor)

    assert output.shape == (3, input_dimension)
    output_numpy = output.numpy()
    assert np.all(output_numpy >= 0.0)
    assert np.all(output_numpy <= 1.0)


def test_variable_selection_network_consistency() -> None:
    input_dimension = 16
    hidden_size = 32

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    input_tensor = Tensor(rng.standard_normal((2, input_dimension)).astype(np.float32))

    first_output = vsn.forward(input_tensor)
    second_output = vsn.forward(input_tensor)

    assert first_output.shape == second_output.shape
    assert np.allclose(first_output.numpy(), second_output.numpy(), rtol=1e-5)


def test_variable_selection_network_single_dimension() -> None:
    input_dimension = 1
    hidden_size = 2

    vsn = VariableSelectionNetwork(
        input_dimension=input_dimension,
        hidden_size=hidden_size,
    )

    input_tensor = Tensor([[1.5], [2.0], [-0.5]])
    output = vsn.forward(input_tensor)

    assert output.shape == (3, 1)
    output_numpy = output.numpy()
    assert np.all(output_numpy >= 0.0)
    assert np.all(output_numpy <= 1.0)
