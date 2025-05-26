from typing import Dict
from category_encoders import OrdinalEncoder
from .ticker_embedding import TickerEmbedding
from .long_short_term_memory import LongShortTermMemory
from .gated_residual_network import GatedResidualNetwork
from .multi_head_self_attention import MultiHeadSelfAttention
from .post_processor import PostProcessor
from tinygrad.tensor import Tensor
from tinygrad.nn.optim import Adam
from tinygrad.nn.state import (
    get_parameters,
    get_state_dict,
    safe_save,
    safe_load,
    load_state_dict,
)
from typing import Tuple, List
import numpy as np
from .dataset import DataSet
from .loss_function import quantile_loss


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
        means_by_ticker: Dict[str, Tensor],
        standard_deviations_by_ticker: Dict[str, Tensor],
        ticker_encoder: OrdinalEncoder,
        dropout_rate: float,  # non-zero indicates training
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

        self.parameters = get_parameters(self)

    def forward(
        self,
        tickers: Tensor,
        features: Tensor,
    ) -> Tuple[Tensor, Tensor, Tuple[np.ndarray, np.ndarray, np.ndarray]]:
        ticker_embeddings = self.ticker_embedding.forward(tickers)

        lstm_input = features.cat(ticker_embeddings, dim=-1)

        lstm_output, _ = self.lstm_encoder.forward(lstm_input)

        processed_features = self.feature_processor.forward(lstm_output)

        attention_output, attention_weights = self.self_attention.forward(
            processed_features
        )

        output = self.output_layer.forward(attention_output)

        percentile_25, percentile_50, percentile_75 = (
            self.post_processor.post_process_predictions(
                tickers.numpy(),
                output.numpy(),
            )
        )

        return output, attention_weights, (percentile_25, percentile_50, percentile_75)

    def train(
        self,
        dataset: DataSet,
        epoch_count: int,
        learning_rate: float = 1e-3,
    ) -> List[float]:
        optimizer = Adam(params=self.parameters, lr=learning_rate)

        quantiles = (0.25, 0.50, 0.75)
        losses: List[float] = []

        for _ in range(epoch_count):
            epoch_loss = 0.0

            for batch in dataset.batches():
                for tickers, historical_features, targets in batch:
                    predictions, _, _ = self.forward(
                        Tensor(tickers),
                        Tensor(historical_features),
                    )

                    loss = quantile_loss(predictions, Tensor(targets), quantiles)

                    optimizer.zero_grad()
                    loss.backward()
                    optimizer.step()

                    epoch_loss += loss.numpy().item()

                avgerage_epoch_loss = epoch_loss / len(dataset)
                losses.append(avgerage_epoch_loss)

        return losses

    def validate(
        self,
        dataset: DataSet,
    ) -> float:
        total_loss = 0.0
        batch_count = len(dataset)

        for batch in dataset.batches():
            tickers, features, targets = batch
            tickers, features, targets = (
                Tensor(tickers),
                Tensor(features),
                Tensor(targets),
            )

            output, _, _ = self.forward(tickers, features)

            loss = quantile_loss(output, targets)

            total_loss += loss.item()

        average_loss = total_loss / batch_count

        return average_loss

    def save(
        self,
        path_and_file: str = "miniature_temporal_fusion_transformer.safetensor",
    ) -> None:
        states = get_state_dict(self)
        safe_save(states, path_and_file)

    def load(
        self,
        path_and_file: str = "miniature_temporal_fusion_transformer.safetensor",
    ) -> None:
        states = safe_load(path_and_file)
        load_state_dict(self, states)

    def predict(
        self,
        tickers: Tensor,
        input: Tensor,
    ) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        predictions, _, _ = self.forward(tickers, input)

        percentile_25, percentile_50, percentile_75 = (
            self.post_processor.post_process_predictions(
                tickers.numpy(),
                predictions.numpy(),
            )
        )

        return percentile_25, percentile_50, percentile_75
