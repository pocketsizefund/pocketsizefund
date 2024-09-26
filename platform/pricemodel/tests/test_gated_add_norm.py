from pricemodel.grn import GatedAddNorm
from tinygrad.tensor import Tensor
from hypothesis import given
import hypothesis.strategies as st
from hypothesis.extra.numpy import arrays, array_shapes, floating_dtypes


@st.composite
def tensors(draw):
    return Tensor(draw(arrays(dtype=floating_dtypes(), shape=array_shapes(min_dims=2, max_dims=2, min_side=1))))


@given(tensors())
def test_gated_add_norm_shape(input_tensor):
    input_dim, output_dim = input_tensor.shape[1], 3
    gan = GatedAddNorm(input_dim, output_dim, dropout=0.1)
    result = gan(input_tensor)
    assert result.shape == (input_tensor.shape[0], output_dim)


@given(tensors())
def test_gated_add_norm_layer_norm_effect(input_tensor):
    input_dim, output_dim = input_tensor.shape[1], 3
    gan = GatedAddNorm(input_dim, output_dim, dropout=0.1)
    result = gan(input_tensor)
    assert abs(result.mean().item()) < 1e-6 and abs(result.std().item() - 1) < 1e-6


@given(tensors())
def test_gated_add_norm_different_dropout(input_tensor):
    input_dim, output_dim = input_tensor.shape[1], 3
    gan1 = GatedAddNorm(input_dim, output_dim, dropout=0.1)
    gan2 = GatedAddNorm(input_dim, output_dim, dropout=0.5)
    result1 = gan1(input_tensor)
    result2 = gan2(input_tensor)
    assert (abs(result1 - result2).mean().item() > 1e-6)  # Expect different results due to different dropout


@given(tensors())
def test_gated_add_norm_deterministic(input_tensor):
    input_dim, output_dim = input_tensor.shape[1], 3
    gan = GatedAddNorm(input_dim, output_dim, dropout=0.0)
    result1 = gan(input_tensor)
    result2 = gan(input_tensor)
    assert (abs(result1 - result2).mean().item() < 1e-6)  # Expect identical results with no dropout


@given(tensors())
def test_gated_add_norm_glu_effect(input_tensor):
    input_dim, output_dim = input_tensor.shape[1], 3
    gan = GatedAddNorm(input_dim, output_dim, dropout=0.0)
    glu_output = gan.glu(input_tensor)
    final_output = gan(input_tensor)
    assert (abs(glu_output - final_output).mean().item() > 1e-6)  # Expect different outputs
