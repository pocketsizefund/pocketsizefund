from typing import cast

from tinygrad.dtype import dtypes
from tinygrad.nn import Linear
from tinygrad.tensor import Tensor


class MultiHeadSelfAttention:
    def __init__(
        self,
        heads_count: int,
        embedding_size: int,
    ) -> None:
        if embedding_size % heads_count != 0:
            message = "Embedding dimension must be divisible by heads count"
            raise ValueError(message)

        self.heads_count: int = heads_count
        self.embedding_size: int = embedding_size
        self.heads_dimension: int = embedding_size // heads_count

        self.query_weight: Linear = Linear(self.embedding_size, self.embedding_size)
        self.key_weight: Linear = Linear(self.embedding_size, self.embedding_size)
        self.value_weight: Linear = Linear(self.embedding_size, self.embedding_size)

        self.fully_connected_out: Linear = Linear(
            self.embedding_size, self.embedding_size
        )

        self.scale: Tensor = Tensor(self.heads_dimension**0.5, dtype=dtypes.float32)

    def forward(
        self,
        input_: Tensor,
    ) -> tuple[Tensor, Tensor]:
        batch_size, sequence_length, _ = input_.shape

        query_weights = self.query_weight(input_)
        key_weights = self.key_weight(input_)
        value_weights = self.value_weight(input_)

        query_weights = query_weights.view(
            (batch_size, sequence_length, self.heads_count, self.heads_dimension),
        ).transpose(1, 2)
        key_weights = key_weights.view(
            (batch_size, sequence_length, self.heads_count, self.heads_dimension),
        ).transpose(1, 2)
        value_weights = value_weights.view(
            (batch_size, sequence_length, self.heads_count, self.heads_dimension),
        ).transpose(1, 2)

        attention_scores = (
            query_weights.matmul(key_weights.transpose(-2, -1)) / self.scale
        )

        attention_weights: Tensor = cast("Tensor", attention_scores).softmax(axis=-1)

        attention_output = attention_weights.matmul(value_weights)

        attention_output = attention_output.transpose(1, 2).reshape(
            batch_size, sequence_length, self.embedding_size
        )

        output = self.fully_connected_out(attention_output)

        return output, attention_weights
