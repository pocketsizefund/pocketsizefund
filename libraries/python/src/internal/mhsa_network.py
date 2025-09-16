from tinygrad.dtype import dtypes
from tinygrad.nn import Linear
from tinygrad.tensor import Tensor


class MultiHeadSelfAttentionNetwork:
    def __init__(
        self,
        heads_count: int,
        embedding_size: int,
        dropout_rate: float = 0.0,
    ) -> None:
        if embedding_size % heads_count != 0:
            message = "Embedding dimension must be divisible by heads count"
            raise ValueError(message)

        self.heads_count = heads_count
        self.embedding_size = embedding_size
        self.heads_dimension = embedding_size // heads_count
        self.dropout_rate = dropout_rate

        self.query_weight = Linear(self.embedding_size, self.embedding_size)
        self.key_weight = Linear(self.embedding_size, self.embedding_size)
        self.value_weight = Linear(self.embedding_size, self.embedding_size)

        self.fully_connected_out = Linear(self.embedding_size, self.embedding_size)

        self.scale = Tensor(self.heads_dimension**0.5, dtype=dtypes.float32)

    def forward(self, inputs: Tensor) -> tuple[Tensor, Tensor]:
        batch_size, sequence_length, _ = inputs.shape

        query_weights = self.query_weight(inputs)
        key_weights = self.key_weight(inputs)
        value_weights = self.value_weight(inputs)

        shape = (batch_size, sequence_length, self.heads_count, self.heads_dimension)

        # shape: (batch, heads_count, sequence_length, head_dimension) # noqa: ERA001
        query_weights = query_weights.view(shape).transpose(1, 2)
        key_weights = key_weights.view(shape).transpose(1, 2)
        value_weights = value_weights.view(shape).transpose(1, 2)

        attention_scores = query_weights.matmul(key_weights.transpose(-2, -1)).div(
            self.scale
        )

        attention_weights = attention_scores.softmax(axis=-1)

        if self.dropout_rate > 0:
            attention_weights = attention_weights.dropout(self.dropout_rate)

        attention_output = attention_weights.matmul(value_weights)

        attention_output = attention_output.transpose(1, 2).reshape(
            batch_size, sequence_length, self.embedding_size
        )

        output = self.fully_connected_out(attention_output)

        if self.dropout_rate > 0:
            output = output.dropout(self.dropout_rate)

        return output, attention_weights
