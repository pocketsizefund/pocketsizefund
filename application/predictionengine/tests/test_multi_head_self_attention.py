import numpy as np
from tinygrad.tensor import Tensor

from application.predictionengine.src.predictionengine.multi_head_self_attention import (  # noqa: E501
    MultiHeadSelfAttention,
)


def test_multi_head_attention_initialization() -> None:
    attention = MultiHeadSelfAttention(heads_count=8, embedding_size=64)

    assert attention.heads_count == 8  # noqa: PLR2004
    assert attention.embedding_size == 64  # noqa: PLR2004


def test_multi_head_attention_forward() -> None:
    attention = MultiHeadSelfAttention(heads_count=4, embedding_size=32)

    default_range = np.random.default_rng()
    input_tensor = Tensor(default_range.standard_normal((2, 10, 32)))
    output, attention_weights = attention.forward(input_tensor)

    assert output.shape == (2, 10, 32)
    assert attention_weights.shape[0] == 2  # noqa: PLR2004 batch size
    assert attention_weights.shape[1] == 4  # noqa: PLR2004 heads count


def test_multi_head_attention_different_heads() -> None:
    for heads_count in [1, 2, 4, 8]:
        embedding_size = 32
        attention = MultiHeadSelfAttention(
            heads_count=heads_count, embedding_size=embedding_size
        )

        default_range = np.random.default_rng()
        input_tensor = Tensor(default_range.standard_normal((1, 5, embedding_size)))
        output, attention_weights = attention.forward(input_tensor)

        assert output.shape == (1, 5, embedding_size)
        assert attention_weights.shape[1] == heads_count


def test_multi_head_attention_single_sequence() -> None:
    attention = MultiHeadSelfAttention(heads_count=2, embedding_size=16)

    default_range = np.random.default_rng()
    input_tensor = Tensor(default_range.standard_normal((1, 1, 16)))
    output, _ = attention.forward(input_tensor)

    assert output.shape == (1, 1, 16)


def test_multi_head_attention_longer_sequences() -> None:
    attention = MultiHeadSelfAttention(heads_count=4, embedding_size=64)

    for sequence_length in [10, 20, 50]:
        default_range = np.random.default_rng()
        input_tensor = Tensor(default_range.standard_normal((1, sequence_length, 64)))
        output, _ = attention.forward(input_tensor)

        assert output.shape == (1, sequence_length, 64)


def test_multi_head_attention_batch_processing() -> None:
    attention = MultiHeadSelfAttention(heads_count=2, embedding_size=32)

    for batch_size in [1, 2, 4, 8]:
        default_range = np.random.default_rng()
        input_tensor = Tensor(default_range.standard_normal((batch_size, 5, 32)))
        output, attention_weights = attention.forward(input_tensor)

        assert output.shape == (batch_size, 5, 32)
        assert attention_weights.shape[0] == batch_size
