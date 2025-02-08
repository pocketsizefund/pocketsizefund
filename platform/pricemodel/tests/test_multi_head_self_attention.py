from pricemodel.multi_head_self_attention import MultiHeadSelfAttention
from tinygrad import Tensor


def test_multi_head_self_attention():
    heads_count = 4
    embedding_size = 16

    multi_head_self_attention = MultiHeadSelfAttention(
        heads_count=heads_count,
        embedding_size=embedding_size,
    )

    assert multi_head_self_attention.heads_count == heads_count, "Mismatch in heads count"
    assert multi_head_self_attention.embedding_size == embedding_size, "Mismatch in embedding size"
    assert (
        multi_head_self_attention.heads_dimension == embedding_size // heads_count
    ), "Mismatch in heads dimension"

    batch_size = 5
    sequence_length = 30
    feature_size = 16

    input = Tensor.zeros(batch_size, sequence_length, feature_size)

    output, attention_weights = multi_head_self_attention.forward(input)

    assert output.shape == (batch_size, sequence_length, embedding_size)
    assert attention_weights.shape == (batch_size, heads_count, sequence_length, sequence_length)
