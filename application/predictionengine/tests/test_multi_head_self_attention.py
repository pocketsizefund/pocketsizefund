from tinygrad.tensor import Tensor
import numpy as np
from application.predictionengine.src.predictionengine.multi_head_self_attention import (
    MultiHeadSelfAttention,
)


def test_multi_head_attention_initialization():
    attention = MultiHeadSelfAttention(heads_count=8, embedding_size=64)

    assert attention.heads_count == 8
    assert attention.embedding_size == 64


def test_multi_head_attention_forward():
    attention = MultiHeadSelfAttention(heads_count=4, embedding_size=32)

    input_tensor = Tensor(np.random.randn(2, 10, 32))
    output, attention_weights = attention.forward(input_tensor)

    assert output.shape == (2, 10, 32)
    assert attention_weights.shape[0] == 2  # batch size
    assert attention_weights.shape[1] == 4  # heads count


def test_multi_head_attention_different_heads():
    for heads_count in [1, 2, 4, 8]:
        embedding_size = 32
        attention = MultiHeadSelfAttention(
            heads_count=heads_count, embedding_size=embedding_size
        )

        input_tensor = Tensor(np.random.randn(1, 5, embedding_size))
        output, attention_weights = attention.forward(input_tensor)

        assert output.shape == (1, 5, embedding_size)
        assert attention_weights.shape[1] == heads_count


def test_multi_head_attention_single_sequence():
    attention = MultiHeadSelfAttention(heads_count=2, embedding_size=16)

    input_tensor = Tensor(np.random.randn(1, 1, 16))
    output, _ = attention.forward(input_tensor)

    assert output.shape == (1, 1, 16)


def test_multi_head_attention_longer_sequences():
    attention = MultiHeadSelfAttention(heads_count=4, embedding_size=64)

    for seq_len in [10, 20, 50]:
        input_tensor = Tensor(np.random.randn(1, seq_len, 64))
        output, _ = attention.forward(input_tensor)

        assert output.shape == (1, seq_len, 64)


def test_multi_head_attention_batch_processing():
    attention = MultiHeadSelfAttention(heads_count=2, embedding_size=32)

    for batch_size in [1, 2, 4, 8]:
        input_tensor = Tensor(np.random.randn(batch_size, 5, 32))
        output, attention_weights = attention.forward(input_tensor)

        assert output.shape == (batch_size, 5, 32)
        assert attention_weights.shape[0] == batch_size
