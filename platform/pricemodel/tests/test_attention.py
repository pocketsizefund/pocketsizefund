from pricemodel.attention import (
    bmm,
    masked_fill,
    tensor_astype,
    ScaledDotProductAttention,
    InterpretableMultiHeadAttention,
)
from tinygrad import Tensor
from tinygrad.dtype import dtypes


def test_bmm():
    A = Tensor.randn(2, 3, 4)
    B = Tensor.randn(2, 4, 5)

    result = bmm(A, B)

    assert result.shape == (2, 3, 5)


def test_masked_fill():
    tensor = Tensor([[1.0, 2.0], [3.0, 4.0]])
    mask = Tensor([[1, 0], [0, 1]])
    value = -99.0

    expected_output = Tensor([[-99.0, 2.0], [3.0, -99.0]])

    received_output = masked_fill(tensor, mask, value)

    assert (
        received_output.tolist() == expected_output.tolist()
    ), f"Expected {expected_output}, but got {received_output}"


def test_tensor_astype():
    tensor = Tensor([1, 2, 3])
    dtype = dtypes.float32

    result = tensor_astype(tensor, dtype)

    assert (
        result.tolist() == Tensor([1.0, 2.0, 3.0]).tolist()
    ), f"Expected Tensor([1.0, 2.0, 3.0]), but got {result}"


def test_scaled_dot_production_attention():
    scaled_dot_product_attention = ScaledDotProductAttention()

    batch_size = 2
    num_queries = 4
    num_keys = 6
    d_k = 8
    d_v = 10

    q = Tensor.randn(batch_size, num_queries, d_k)
    k = Tensor.randn(batch_size, num_keys, d_k)
    v = Tensor.randn(batch_size, num_keys, d_v)

    output, attention = scaled_dot_product_attention.forward(q, k, v)

    assert output.shape == (2, 4, 10)
    assert attention.shape == (2, 4, 6)


def test_interpretable_multi_head_attention():
    heads_count = 8
    models_count = 128
    dropout_rate = 0.1

    imha = InterpretableMultiHeadAttention(
        heads_count=heads_count,
        models_count=models_count,
        dropout_rate=dropout_rate,
    )

    assert imha.heads_count == 8
    assert imha.models_count == 128
    assert imha.dropout_rate == 0.1

    assert imha.keys_dimensions == imha.queries_dimensions == imha.values_dimensions == 128 // 8
    assert len(imha.keys_layers) == 8
    assert len(imha.queries_layers) == 8

    batch_size = 32
    sequence_length = 10
    models_count = 128

    q = Tensor.randn(batch_size, sequence_length, models_count)
    k = Tensor.randn(batch_size, sequence_length, models_count)
    v = Tensor.randn(batch_size, sequence_length, models_count)

    mask = None

    outputs, attention_scores = imha.forward(q, k, v, mask)

    assert outputs.shape == (32, 10, 128)
    assert attention_scores.shape == (32, 10, 8, 10)
