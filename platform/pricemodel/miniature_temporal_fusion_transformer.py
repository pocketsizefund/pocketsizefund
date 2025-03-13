from typing import Dict
from category_encoders import OrdinalEncoder
from pricemodel.ticker_embedding import TickerEmbedding
from pricemodel.long_short_term_memory import LongShortTermMemory
from pricemodel.gated_residual_network import GatedResidualNetwork
from pricemodel.multi_head_self_attention import MultiHeadSelfAttention
from pricemodel.post_processor import PostProcessor
from tinygrad import Tensor
from tinygrad.nn import Linear
from tinygrad.nn.optim import Adam
from tinygrad.nn.state import get_parameters, get_state_dict, safe_save, safe_load, load_state_dict
from typing import Tuple, List
import numpy as np
from pricemodel.dataset import DataSet
from pricemodel.loss_function import quantile_loss


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
        dropout_rate: float,  # non-zero indicates training
    ) -> None:
        self.ticker_embedding = TickerEmbedding(
            ticker_count=ticker_count,
            embedding_size=embedding_size,
        )

        self.lstm_encoder = LongShortTermMemory(
            input_size=input_size,
            hidden_size=hidden_size,
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
            output_size=hidden_size,
        )

        self.projection_layer = Linear(
            in_features=hidden_size,
            out_features=output_size,  # NOTE: received from parameters
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
    ) -> Tensor:
        print("minitft - tickers:", tickers.tolist())  # TEMP
        print("minitft - features:", features.shape)  # TEMP

        sequence_length = features.shape[1]

        ticker_embeddings = self.ticker_embedding.forward(tickers)
        ticker_embeddings = ticker_embeddings.unsqueeze(1).repeat(1, sequence_length, 1)

        lstm_input = features.cat(ticker_embeddings, dim=-1).realize()  # keep realize

        lstm_output, _ = self.lstm_encoder.forward(lstm_input)

        processed_features = self.feature_processor.forward(lstm_output)

        # NOTE: return attention weights
        attention_output, attention_weights = self.self_attention.forward(processed_features)

        output_layer_output = self.output_layer.forward(attention_output)

        # NOTE: this should be changed to the first 5 days (not the last 5 days)
        # NOTE: confirm that this is even actually needed
        projection_output = self.projection_layer(output_layer_output[:, -5:, :])  # (30, 5, 1)

        projection_output = projection_output.realize()  # keep realize

        print("minitft - projection_output 1:", projection_output.shape)  # TEMP

        projection_output = projection_output.squeeze(-1)  # (30, 5)

        print("minitft - projection_output 2:", projection_output.shape)  # TEMP

        # NOTE: replace with return value
        prediction_percentiles_by_tickers = self.post_processor.process_predictions(
            tickers=tickers,
            predictions=projection_output,
        )

        print(
            "minitft - prediction_percentiles_by_tickers:", prediction_percentiles_by_tickers
        )  # TEMP

        return prediction_percentiles_by_tickers

    # --------------------------------------------------------------------------------------------

    # def train(self, dataset: DataSet, epoch_count: int, learning_rate: float = 1e-3) -> List[float]:
    #     all_params = get_parameters(self)
    #     print(f"Total parameters before filtering: {len(all_params)}")
    #     for i, p in enumerate(all_params):
    #         print(
    #             f"Param {i}: shape {p.shape}, requires_grad: {getattr(p, 'requires_grad', 'Not Set')}"
    #         )

    #     # Treat None as True for weights, exclude explicit False
    #     params = [
    #         p.reshape(1) if p.shape == () else p for p in all_params if p.requires_grad is not False
    #     ]
    #     print(f"Total trainable params: {len(params)}")
    #     chunk_size = 1
    #     optimizers = []
    #     for i in range(0, len(params), chunk_size):
    #         chunk = params[i : i + chunk_size]
    #         print(f"Chunk {i // chunk_size}: start {i}, end {i + chunk_size}, size {len(chunk)}")
    #         if len(chunk) == 0:
    #             raise ValueError(f"Empty chunk at index {i}")
    #         optimizers.append(Adam(params=chunk, lr=learning_rate))
    #     if not optimizers:
    #         raise ValueError("No valid optimizer chunks created")
    #     losses = []
    #     for epoch in range(epoch_count):
    #         print("mini-tft - epoch:", epoch)
    #         epoch_loss = 0.0
    #         for tickers, historical_features, targets in dataset:
    #             predictions = self.forward(tickers=tickers, features=historical_features)
    #             loss = quantile_loss(predictions, targets, tickers)
    #             for opt in optimizers:
    #                 opt.zero_grad()
    #             loss.backward()
    #             for i, opt in enumerate(optimizers):
    #                 print(f"Stepping optimizer chunk {i}, params: {len(opt.params)}")
    #                 opt.step()
    #             epoch_loss += loss.numpy().item()
    #         avg_epoch_loss = epoch_loss / len(dataset)
    #         losses.append(avg_epoch_loss)
    #     return losses

    # --------------------------------------------------------------------------------------------

    # def train(self, dataset: DataSet, epoch_count: int, learning_rate: float = 1e-3) -> List[float]:
    #     all_params = get_parameters(self)
    #     print(f"Total parameters before filtering: {len(all_params)}")
    #     for i, p in enumerate(all_params):
    #         print(
    #             f"Param {i}: shape {p.shape}, requires_grad: {getattr(p, 'requires_grad', 'Not Set')}"
    #         )

    #     params = [
    #         p.reshape(1) if p.shape == () else p
    #         for p in all_params
    #         if getattr(p, "requires_grad", False)
    #     ]
    #     print(f"Total trainable params: {len(params)}")
    #     chunk_size = 20
    #     optimizers = []
    #     for i in range(0, len(params), chunk_size):
    #         chunk = params[i : i + chunk_size]
    #         print(f"Chunk {i // chunk_size}: start {i}, end {i + chunk_size}, size {len(chunk)}")
    #         if len(chunk) == 0:
    #             raise ValueError(f"Empty chunk at index {i}")
    #         optimizers.append(Adam(params=chunk, lr=learning_rate))
    #     if not optimizers:
    #         raise ValueError("No valid optimizer chunks created")
    #     losses = []
    #     for epoch in range(epoch_count):
    #         print("mini-tft - epoch:", epoch)
    #         epoch_loss = 0.0
    #         for tickers, historical_features, targets in dataset:
    #             predictions = self.forward(tickers=tickers, features=historical_features)
    #             loss = quantile_loss(predictions, targets, tickers)
    #             for opt in optimizers:
    #                 opt.zero_grad()
    #             loss.backward()
    #             for i, opt in enumerate(optimizers):
    #                 print(f"Stepping optimizer chunk {i}, params: {len(opt.params)}")
    #                 opt.step()
    #             epoch_loss += loss.numpy().item()
    #         avg_epoch_loss = epoch_loss / len(dataset)
    #         losses.append(avg_epoch_loss)
    #     return losses

    # --------------------------------------------------------------------------------------------

    def train(
        self,
        dataset: DataSet,
        epoch_count: int,
        learning_rate: float = 1e-3,
    ) -> List[float]:
        # NOTE: see comment on multi-head self attention regarding reshaping hack
        params = [p.reshape(1) if p.shape == () else p for p in self.parameters]
        optimizer = Adam(params=params, lr=learning_rate)
        # optimizer = Adam(params=self.parameters, lr=learning_rate)

        quantiles = (0.25, 0.50, 0.75)  # NOTE: remove
        losses: List[float] = []

        for _ in range(epoch_count):
            epoch_loss = 0.0

            for tickers, historical_features, targets in dataset:
                predictions = self.forward(  # NOTE: potentially change to multiple outputs
                    tickers=tickers,
                    features=historical_features,
                )

                loss = quantile_loss(predictions, targets, tickers, quantiles)  # TEMP

                optimizer.zero_grad()

                loss.backward()

                optimizer.step()

                print("minitft - AFTER STEP")  # TEMP

                epoch_loss += loss.numpy().item()

            avgerage_epoch_loss = epoch_loss / len(dataset)
            losses.append(avgerage_epoch_loss)

        print("minitft - losses:", losses)  # TEMP

        return losses

    def validate(
        self,
        dataset: DataSet,
    ) -> float:
        total_loss = 0.0
        batch_count = len(dataset)

        for batch in dataset:
            tickers, features, targets = batch
            tickers, features, targets = Tensor(tickers), Tensor(features), Tensor(targets)

            output = self.forward(tickers, features)

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

    @staticmethod
    def load(
        self,
        path_and_file: str = "miniature_temporal_fusion_transformer.safetensor",
    ) -> None:
        states = safe_load(path_and_file)
        load_state_dict(self, states)

    def predict(
        self,
        dataset: DataSet,
    ) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        for tickers, historical_features, _ in dataset:
            predictions, _, _ = self.forward(
                tickers=tickers,
                features=historical_features,
            )

            # NOTE: temporary
            percentiles_by_ticker = self.post_processor.process_predictions(
                tickers,
                predictions,
            )

            break  # only one batch expected

        print("minitft - percentiles_by_ticker:", percentiles_by_ticker)

        return percentiles_by_ticker
