from typing import Dict
from category_encoders import OrdinalEncoder
from pricemodel.ticker_embedding import TickerEmbedding
from pricemodel.long_short_term_memory import LongShortTermMemory
from pricemodel.gated_residual_network import GatedResidualNetwork
from pricemodel.multi_head_self_attention import MultiHeadSelfAttention
from pricemodel.post_processor import PostProcessor


class MiniatureTemporalFusionTransformer:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        output_size: int,
        layer_count: int,
        ticker_count: int,
        embedding_size: int,
        attention_head_count: int,
        means_by_ticker: Dict[str, float],
        standard_deviations_by_ticker: Dict[str, float],
        ticker_encoder: OrdinalEncoder,
        dropout_rate: float,
    ) -> None:
        self.ticker_embedding = TickerEmbedding(
            ticker_count=ticker_count,
            embedding_size=embedding_size,
        )

        self.lstm_encoder = LongShortTermMemory(
            input_size=input_size,
            hidden_size=hidden_size + embedding_size,
            layer_count=layer_count,
            dropout_rate=dropout_rate,
        )

        self.feature_processor = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
        )

        self.self_attention = MultiHeadSelfAttention(
            heads_count=attention_head_count,
            embedding_size=hidden_size,
        )

        self.output_layer = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=output_size,
        )

        self.post_processor = PostProcessor(
            means_by_ticker=means_by_ticker,
            standard_deviations_by_ticker=standard_deviations_by_ticker,
            ticker_encoder=ticker_encoder,
        )

    def forward(self):
        pass

    def train(self):
        pass

    def validate(self):
        pass

    def save(self):
        pass

    def load(self):
        pass

    def predict(self):
        pass
