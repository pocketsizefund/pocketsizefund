import numpy as np
import numpy.typing as npt
from category_encoders import OrdinalEncoder
from tinygrad.nn.optim import Adam
from tinygrad.nn.state import (
    get_parameters,
    get_state_dict,
    load_state_dict,
    safe_load,
    safe_save,
)
from tinygrad.tensor import Tensor

from .dataset import DataSet
from .gated_residual_network import GatedResidualNetwork
from .long_short_term_memory import LongShortTermMemory
from .loss_function import quantile_loss
from .multi_head_self_attention import MultiHeadSelfAttention
from .post_processor import PostProcessor
from .ticker_embedding import TickerEmbedding


class MiniatureTemporalFusionTransformer:
    def __init__(  # noqa: PLR0913
        self,
        input_size: int,
        hidden_size: int,
        output_size: int,
        layer_count: int,
        ticker_count: int,
        embedding_size: int,
        attention_head_count: int,
        means_by_ticker: dict[str, Tensor],
        standard_deviations_by_ticker: dict[str, Tensor],
        ticker_encoder: OrdinalEncoder,
        dropout_rate: float,  # non-zero indicates training
    ) -> None:
        self.ticker_embedding: TickerEmbedding = TickerEmbedding(
            ticker_count=ticker_count,
            embedding_size=embedding_size,
        )

        self.lstm_encoder: LongShortTermMemory = LongShortTermMemory(
            input_size=input_size + embedding_size,
            hidden_size=hidden_size,
            layer_count=layer_count,
            dropout_rate=dropout_rate,
        )

        self.feature_processor: GatedResidualNetwork = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
        )

        self.self_attention: MultiHeadSelfAttention = MultiHeadSelfAttention(
            heads_count=attention_head_count,
            embedding_size=hidden_size,
        )

        self.output_layer: GatedResidualNetwork = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=output_size,
        )

        self.post_processor: PostProcessor = PostProcessor(
            means_by_ticker=means_by_ticker,
            standard_deviations_by_ticker=standard_deviations_by_ticker,
            ticker_encoder=ticker_encoder,
        )

        self.parameters = get_parameters(self)

    def forward(
        self,
        tickers: Tensor,
        features: Tensor,
    ) -> tuple[
        Tensor,
        Tensor,
        tuple[
            npt.NDArray[np.float64], npt.NDArray[np.float64], npt.NDArray[np.float64]
        ],
    ]:
        ticker_embeddings = self.ticker_embedding.forward(
            tickers
        )  # (batch_size, embedding_dim)
        ticker_embeddings = ticker_embeddings.unsqueeze(1).expand(
            -1, features.size(1), -1
        )  # (batch size, sequence length, embedding dimension)
        lstm_input = Tensor.cat(features, ticker_embeddings, dim=-1)

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
    ) -> list[float]:
        optimizer = Adam(params=self.parameters, lr=learning_rate)

        quantiles: tuple[float, float, float] = (0.25, 0.50, 0.75)
        losses: list[float] = []

        for _ in range(epoch_count):
            epoch_loss: float = 0.0

            for tickers, historical_features, targets in dataset.batches():
                predictions, _, _ = self.forward(
                    tickers,
                    historical_features,
                )

                loss: Tensor = quantile_loss(predictions, targets, quantiles)

                optimizer.zero_grad()
                _ = loss.backward()
                optimizer.step()

                epoch_loss += loss.numpy().item()

                avgerage_epoch_loss: float = epoch_loss / len(dataset)
                losses.append(avgerage_epoch_loss)

        return losses

    def validate(
        self,
        dataset: DataSet,
    ) -> float:
        total_loss = 0.0
        batch_count = 0

        for tickers, features, targets in dataset.batches():
            output, _, _ = self.forward(tickers, features)
            loss = quantile_loss(output, targets)
            total_loss += loss.item()
            batch_count += 1

        return total_loss / batch_count

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
        _ = load_state_dict(self, states)

    def predict(
        self,
        tickers: Tensor,
        input_: Tensor,
    ) -> tuple[
        npt.NDArray[np.float64], npt.NDArray[np.float64], npt.NDArray[np.float64]
    ]:
        predictions, _, _ = self.forward(tickers, input_)

        percentile_25, percentile_50, percentile_75 = (
            self.post_processor.post_process_predictions(
                tickers.numpy(),
                predictions.numpy(),
            )
        )

        return percentile_25, percentile_50, percentile_75
