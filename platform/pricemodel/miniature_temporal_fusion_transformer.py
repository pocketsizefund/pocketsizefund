from tinygrad import Tensor
from pricemodel.dataset import DataSet
from pricemodel.new_ticker_embedding import TickerEmbedding
from tinygrad.nn.state import get_parameters
from tinygrad.nn.optim import Adam
from typing import List
from pricemodel.long_short_term_memory import LongShortTermMemory
from pricemodel.gated_residual_network import GatedResidualNetwork
from pricemodel.multi_head_self_attention import MultiHeadSelfAttention
from pricemodel.projection_layer import ProjectionLayer
from pricemodel.loss_function import loss_function


class MiniatureTemporalFusionTransformer:
    def __init__(
        self,
        ticker_count: int,
        embedding_size: int,
        feature_count: int,
        hidden_size: int,
        output_size: int,
        lstm_layer_count: int,
        attention_head_count: int,
        dropout_rate: float = 0.0,
    ) -> None:
        self.input_size = embedding_size + feature_count

        self.ticker_embedding = TickerEmbedding(
            ticker_count=ticker_count,
            embedding_size=embedding_size,
        )

        self.lstm_encoder = LongShortTermMemory(
            input_size=self.input_size,
            hidden_size=hidden_size,
            layer_count=lstm_layer_count,
            dropout_rate=dropout_rate,
        )

        self.feature_processor = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
        )

        self.self_attention = MultiHeadSelfAttention(
            hidden_size=hidden_size,
            heads_count=attention_head_count,
            embedding_size=hidden_size,
        )

        self.output_layer = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
        )

        self.projection_layer = ProjectionLayer(
            input_size=hidden_size,
            output_size=output_size,
        )

        self.parameters = get_parameters(self)

    def forward(
        self,
        tickers: Tensor,
        features: Tensor,
    ) -> Tensor:
        batch_size, sequence_length, feature_size = features.shape

        ticker_embeddings = self.ticker_embedding.forward(tickers)
        ticker_embeddings = ticker_embeddings.unsqueeze(1).repeat(1, sequence_length, 1)

        x = Tensor.cat(ticker_embeddings, features, dim=-1)

        x, _ = self.lstm_encoder.forward(x)  # [batch_size, sequence_length, hidden_dim]
        x = self.feature_processor.forward(x)  # [batch_size, sequence_length, hidden_dim]
        x, _ = self.self_attention.forward(x)  # [batch_size, sequence_length, hidden_dim]
        x = self.output_layer.forward(x)  # [batch_size, sequence_length, hidden_dim]
        x = self.projection_layer.forward(x)  # [batch_size, output_dim]

        return x

    def train(
        self,
        dataset: DataSet,
        epoch_count: int,
        learning_rate: float = 1e-3,
    ) -> List[float]:
        optimizer = Adam(params=self.parameters, lr=learning_rate)

        losses: List[float] = []
        for epoch in range(epoch_count):
            for tickers, features, targets in dataset:
                predictions = self.forward(tickers, features)
                loss = loss_function(predictions, targets)
                optimizer.zero_grad()
                loss.backward()
                optimizer.step()
                losses.append(loss.item())

        return losses
