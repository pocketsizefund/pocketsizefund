from hypothesis import note, given, strategies as st, settings
from hypothesis.extra.array_api import make_strategies_namespace

import pytest
import numpy as np
from tinygrad.tensor import Tensor
from typing import Tuple


from components.gated_residual_network import GatedLinearUnit

xp = np
xps = make_strategies_namespace(xp)


@st.composite
def tensor_shapes(draw) -> Tuple[int, int, int]:
    batch_size = draw(st.integers(min_value=1, max_value=64))
    seq_len = draw(st.integers(min_value=1, max_value=32))
    input_dim = draw(st.integers(min_value=1, max_value=128))
    return (batch_size, seq_len, input_dim)


@st.composite
def valid_tensors(draw) -> Tuple[Tensor, Tuple[int, int, int]]:
    shape = draw(xps.array_shapes(min_dims=3, max_dims=3, min_side=1, max_side=64))
    array = draw(
        xps.arrays(dtype=xp.float32, shape=shape, elements={"min_value": -10.0, "max_value": 10.0})
    )

    return Tensor(array)


@given(
    valid_tensors(),
    st.integers(min_value=0, max_value=256),
    st.floats(min_value=0.0, max_value=1.0, exclude_max=True, allow_nan=False),
)
@settings(max_examples=100, deadline=None)
def test_gated_linear_unit(x, hidden_size, dropout_rate):
    note(f"{x=}")
    model = GatedLinearUnit(
        input_size=x.shape[-1], hidden_size=hidden_size, dropout_rate=dropout_rate
    )

    output = model(x)

    # assert gated_linear_unit.fc.weight.shape == (
    #     hidden_size * 2,
    #     input_size,
    # ), "Incorrect input size for Linear"
    # assert gated_linear_unit.fc.bias.shape == (hidden_size * 2,), "Incorrect output size for Linear"
    #
    # x = Tensor([[1.0, 2.0, 3.0, 4.0], [4.0, 5.0, 6.0, 7.0]])  # Shape: (2, 4)
    # output = gated_linear_unit.forward(x)
    #
    # assert output.shape == (2, hidden_size), "Output shape mismatch"
    #
    # gated_linear_unit_no_dropout = GatedLinearUnit(
    #     input_size=input_size, hidden_size=hidden_size, dropout_rate=None
    # )
    # output_no_dropout = gated_linear_unit_no_dropout.forward(x)
    #
    # assert output_no_dropout.shape == (2, hidden_size), "Output shape mismatch without dropout"
