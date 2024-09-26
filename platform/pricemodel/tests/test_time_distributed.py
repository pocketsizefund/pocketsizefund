from hypothesis import given, strategies as st
from tinygrad.tensor import Tensor
from tinygrad import nn


@given(st.integers(min_value=1, max_value=100), st.booleans())
def test_init(output_size, trainable):
    tdi = TimeDistributedInterpolation(output_size, trainable)
    assert tdi.output_size == output_size
    assert tdi.trainable == trainable
    if trainable:
        assert isinstance(tdi.mask, Tensor)
        assert tdi.mask.shape == (output_size,)
        assert tdi.mask.requires_grad
        assert isinstance(tdi.gate, nn.Sigmoid)

@given(st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10))
def test_interpolate_shape(batch_size, input_size):
    tdi = TimeDistributedInterpolation(15)
    x = Tensor.randn(batch_size, input_size)
    result = tdi.interpolate(x)
    assert result.shape == (batch_size, 2, 15)


@given(st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10))
def test_call_2d_input(batch_size, input_size):
    tdi = TimeDistributedInterpolation(15)
    x = Tensor.randn(batch_size, input_size)
    result = tdi(x)
    assert result.shape == (batch_size, 2, 15)

@given(st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10))
def test_call_3d_input(batch_size, timesteps, input_size):
    tdi = TimeDistributedInterpolation(15)
    x = Tensor.randn(batch_size, timesteps, input_size)
    result = tdi(x)
    assert result.shape == (batch_size, timesteps, 2, 15)

@given(st.integers(min_value=1, max_value=100), st.booleans())
def test_trainable_mask(output_size, trainable):
    tdi = TimeDistributedInterpolation(output_size, trainable)
    x = Tensor.randn(1, output_size)
    result = tdi(x)
    assert result.requires_grad == trainable

@given(st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10))
def test_interpolate_values(batch_size, input_size):
    tdi = TimeDistributedInterpolation(15, trainable=False)
    x = Tensor.ones(batch_size, input_size)
    result = tdi.interpolate(x)
    assert (result == 1).all()

@given(st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10))
def test_trainable_interpolate_values(batch_size, input_size):
    tdi = TimeDistributedInterpolation(15, trainable=True)
    x = Tensor.ones(batch_size, input_size)
    result = tdi.interpolate(x)
    assert (result <= 2).all() and (result >= 0).all()

@given(st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10))
def test_call_4d_input(batch_size, channels, height, width):
    tdi = TimeDistributedInterpolation(15)
    x = Tensor.randn(batch_size, channels, height, width)
    result = tdi(x)
    assert result.shape == (batch_size, channels, 2, 15)

@given(st.integers(min_value=1, max_value=100))
def test_large_input(input_size):
    tdi = TimeDistributedInterpolation(15)
    x = Tensor.randn(1, input_size)
    result = tdi(x)
    assert result.shape == (1, 2, 15)

@given(st.integers(min_value=1, max_value=10), st.integers(min_value=1, max_value=10))
def test_gradient_flow(batch_size, input_size):
    tdi = TimeDistributedInterpolation(15, trainable=True)
    x = Tensor.randn(batch_size, input_size)
    result = tdi(x)
    loss = result.sum()
    loss.backward()
    assert tdi.mask.grad is not None
