import polars as pl
from tinygrad import Tensor
from tinygrad.nn import Linear
import numpy as np


class Normalizer:
    def __init__(
        self,
        data: pl.DataFrame,
    ) -> None:
        self.mean = data.mean(axis=0)
        self.standard_deviation = data.std(axis=0)

    def normalize(
        self,
        data: pl.DataFrame,
    ) -> pl.DataFrame:
        return (data - self.mean) / self.standard_deviation

    def save_parameters(
        self,
        path: str,
    ) -> None:
        np.savez(path, mean=self.mean.numpy(), std=self.standard_deviation.numpy())

    def load_parameters(
        self,
        path: str,
    ) -> any:  # TEMP (NEEDS TYPE)
        parameters = np.load(path)
        normalizer = Normalizer(None)
        normalizer.mean = Tensor(parameters["mean"])
        normalizer.standard_deviation = Tensor(parameters["standard_deviation"])
        return normalizer


class DataLoader:
    def __init__(
        self,
        dataframe: pl.DataFrame,
        batch_size: int = 64,
    ) -> None:
        self.dataframe = dataframe
        self.batch_size = batch_size
        self.n_samples = len(dataframe)

    def __iter__(self):
        for i in range(0, self.n_samples, self.batch_size):
            batch = self.dataframe.iloc[i : i + self.batch_size]
            yield Tensor(batch)


class StaticEncoder:
    def __init__(
        self,
        feature_dimensions: int,
        embed_size: int,
    ) -> None:
        self.weights = Tensor.uniform(feature_dimensions, embed_size)

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        return x @ self.weights


class TemporalEncoder:
    def __init__(
        self,
        input_size: int,
        embed_size: int,
    ) -> None:
        self.input_size = input_size
        self.embed_size = embed_size
        self.fc = Linear(input_size, embed_size)

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        return self.fc(x)


class GRN:
    def __init__(
        self,
        input_dimensions: int,
        output_dimensions: int,
    ) -> None:
        self.input_dimensions = input_dimensions
        self.output_dimensions = output_dimensions
        self.linear = Linear(input_dimensions, output_dimensions)
        self.gate = Linear(input_dimensions, output_dimensions)

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        return self.linear(x) * self.gate(x)


class Softmax:
    def forward(self, x: Tensor) -> Tensor:
        return np.exp(x) / np.sum(np.exp(x), axis=-1, keepdims=True)


class MultiHeadAttention:
    def __init__(
        self,
        embed_size: int,
        number_of_heads: int,
    ) -> None:
        self.number_of_heads = number_of_heads
        self.head_dimensions = embed_size // number_of_heads
        self.query = Linear(embed_size, embed_size)
        self.key = Linear(embed_size, embed_size)
        self.value = Linear(embed_size, embed_size)
        self.fc_out = Linear(embed_size, embed_size)

    def forward(
        self,
        query: Tensor,
        key: Tensor,
        value: Tensor,
        mask: Tensor = None,
    ) -> Tensor:
        # query: Tensor of shape (batch_size, seq_len, embed_size)
        # key: Tensor of shape (batch_size, seq_len, embed_size)
        # value: Tensor of shape (batch_size, seq_len, embed_size)
        # mask: Tensor of shape (batch_size, seq_len), optional mask to ignore certain tokens.

        batch_size = query.shape[0]
        query = self.query(query).reshape(
            batch_size,
            -1,
            self.number_of_heads,
            self.head_dimensions,
        )
        key = self.key(key).reshape(
            batch_size,
            -1,
            self.number_of_heads,
            self.head_dimensions,
        )
        value = self.value(value).reshape(
            batch_size,
            -1,
            self.number_of_heads,
            self.head_dimensions,
        )

        attention_scores = np.matmul(
            query,
            key.transpose(0, 1, 3, 2),
        ) / np.sqrt(self.head_dimensions)

        if mask is not None:
            attention_scores += mask * -1e9

        attention_probs = Softmax()(attention_scores)
        attention_output = np.matmul(attention_probs, value)

        attention_output = attention_output.reshape(batch_size, -1, self.num_heads * self.head_dim)
        return self.fc_out(attention_output)


class Decoder:
    def forward(self, x: Tensor) -> Tensor:
        exp_x = np.exp(x - np.max(x, axis=-1, keepdims=True))
        return exp_x / exp_x.sum(axis=-1, keepdims=True)


class OutputLayer:
    def __init__(
        self,
        embed_size: int,
        output_dimensions: int,
    ) -> None:
        self.fc = Linear(embed_size, output_dimensions)

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        return self.fc(x)


class Dropout:
    def __init__(
        self,
        dropout_rate: float,
    ) -> None:
        self.dropout_rate = dropout_rate

    def forward(
        self,
        x: Tensor,
        training: bool = True,
    ) -> Tensor:
        if training:
            mask = Tensor(np.random.binomial(1, 1 - self.dropout_rate, x.shape))
            return x * mask
        return x
