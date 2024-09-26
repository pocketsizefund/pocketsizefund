from pricemodel.grn import GatedLinearUnit
from tinygrad.tensor import Tensor
from hypothesis import given
import hypothesis.strategies as st
from hypothesis.extra.numpy import arrays, array_shapes, floating_dtypes


@st.composite
def tensors(draw):
    return Tensor(draw(arrays(dtype=floating_dtypes(), shape=array_shapes(min_dims=2, max_dims=2, min_side=1))))

@given(tensors())
def test_glu_shape(input_tensor):
    input_dim, output_dim = input_tensor.shape[1], 3
    glu = GatedLinearUnit(input_dim, output_dim, 0.1)
    result = glu(input_tensor)
    assert result.shape == (input_tensor.shape[0], output_dim)



# @given(tensors())
# def test_glu_value_range(input_tensor):
#     """since the gate function passes through the sigmoid,
#     it should always be in [0,1]"""
#     input_dim, output_dim = input_tensor.shape[1], 2
#     glu = GatedLinearUnit(input_dim, output_dim)
#     output = glu(input_tensor)
#
#     gate_output = output.sigmoid(glu.gate(input_tensor))
#
#     assert all(gate_output >= 0) and all(gate_output <= 1)
#


def test_gated_linear_unit():
    output_size = 8
    glu = GatedLinearUnit(16, output_size, 0.1)
    x = Tensor.randn(100, 16)
    result = glu(x)
    assert result.shape == (100, 8)
