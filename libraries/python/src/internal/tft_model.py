from pydantic import BaseModel
from tinygrad.nn import Linear
from tinygrad.nn.optim import Adam
from tinygrad.nn.state import (
    get_parameters,
    get_state_dict,
    load_state_dict,
    safe_load,
    safe_save,
)
from tinygrad.tensor import Tensor

from .loss_functions import quantile_loss
from .lstm_network import LSTM
from .mhsa_network import MultiHeadSelfAttentionNetwork
from .variable_selection_network import VariableSelectionNetwork


class Parameters(BaseModel):
    hidden_size: int = 64
    output_size: int = 1  # closing price
    lstm_layer_count: int = 3
    attention_head_size: int = 4
    dropout_rate: float = 0.1
    quantiles: list[float] = [0.1, 0.5, 0.9]
    decoder_categorical_dimension: int
    decoder_continuous_dimension: int
    encoder_categorical_dimension: int
    encoder_continuous_dimension: int
    static_categorical_dimension: int
    static_continuous_dimension: int
    input_length: int = 35  # five weeks
    output_length: int = 7  # one week


# https://arxiv.org/pdf/1912.09363
class TFTModel:
    def __init__(self, parameters: Parameters) -> None:
        self.hidden_size = parameters.hidden_size
        self.batch_size = parameters.input_length
        self.input_length = parameters.input_length
        self.output_length = parameters.output_length
        self.output_size = parameters.output_size
        self.quantiles = parameters.quantiles

        encoder_dimension = (
            parameters.encoder_categorical_dimension
            + parameters.encoder_continuous_dimension
        )

        decoder_dimension = (
            parameters.decoder_categorical_dimension
            + parameters.decoder_continuous_dimension
        )

        self.encoder_variable_selection_network = VariableSelectionNetwork(
            input_dimension=encoder_dimension,
            hidden_size=parameters.hidden_size,
        )

        self.decoder_variable_selection_network = VariableSelectionNetwork(
            input_dimension=decoder_dimension,
            hidden_size=parameters.hidden_size,
        )

        self.static_context_linear = Linear(
            in_features=parameters.static_categorical_dimension
            + parameters.static_continuous_dimension,
            out_features=parameters.hidden_size,
        )

        self.lstm_encoder = LSTM(
            input_size=encoder_dimension,
            hidden_size=parameters.hidden_size,
            layer_count=parameters.lstm_layer_count,
            dropout_rate=parameters.dropout_rate,
        )

        self.lstm_decoder = LSTM(
            input_size=decoder_dimension,
            hidden_size=parameters.hidden_size,
            layer_count=parameters.lstm_layer_count,
            dropout_rate=parameters.dropout_rate,
        )

        self.self_attention = MultiHeadSelfAttentionNetwork(
            heads_count=parameters.attention_head_size,
            embedding_size=parameters.hidden_size,
            dropout_rate=parameters.dropout_rate,
        )

        self.pre_output_layer = Linear(
            in_features=parameters.hidden_size,
            out_features=parameters.output_size * len(parameters.quantiles),
        )

        self.output_layer = Linear(
            in_features=parameters.hidden_size,
            out_features=parameters.output_size,
        )

        self.parameters = get_parameters(self)

    def forward(self, inputs: dict[str, Tensor]) -> dict[str, Tensor]:
        encoder_categorical_features = inputs["encoder_categorical_features"]
        encoder_continuous_features = inputs["encoder_continuous_features"]

        encoder_input = encoder_categorical_features.cat(
            encoder_continuous_features,
            dim=2,
        )

        decoder_categorical_features = inputs["decoder_categorical_features"]
        decoder_continuous_features = Tensor.zeros(
            self.batch_size, decoder_categorical_features.shape[1], 0
        )  # not currently used

        decoder_input = decoder_categorical_features.cat(
            decoder_continuous_features,
            dim=2,
        )

        static_categorical_features = inputs["static_categorical_features"]

        static_context = None  # NOTE: maybe remove

        static_input = static_categorical_features.to(
            device=self.static_context_linear.weight.device
        )

        static_context = self.static_context_linear(static_input)

        static_context = static_context.view((self.batch_size, self.hidden_size))

        encoder_weights = self.encoder_variable_selection_network.forward(encoder_input)

        encoder_input = Tensor(encoder_input * encoder_weights)

        decoder_weights = self.decoder_variable_selection_network.forward(decoder_input)

        decoder_input = Tensor(decoder_input * decoder_weights)

        encoder_static_context = static_context.unsqueeze(1).expand(
            -1, self.input_length, -1
        )

        decoder_static_context = static_context.unsqueeze(1).expand(
            -1, self.output_length, -1
        )

        encoder_output, (h_n, c_n) = self.lstm_encoder.forward(encoder_input)

        encoder_output = Tensor(encoder_output + encoder_static_context)

        decoder_output, _ = self.lstm_decoder.forward(decoder_input, (h_n, c_n))

        decoder_output = Tensor(decoder_output + decoder_static_context)

        sequence = Tensor.cat(encoder_output, decoder_output, dim=1)

        expanded_static_context = static_context.unsqueeze(1).expand(
            -1, sequence.size(1), -1
        )

        attended_output, _ = self.self_attention.forward(
            Tensor(sequence + expanded_static_context),
        )

        decoder_attended = attended_output[:, -self.output_length :, :]

        output = self.pre_output_layer(decoder_attended).relu()

        predictions = self.output_layer(output)

        quantiles = predictions.reshape(
            self.batch_size,
            self.output_length,
            self.output_size,
            len(self.quantiles),
        )

        return {
            "predictions": quantiles[
                :, :, :, len(self.quantiles) // 2
            ],  # shape: (batch_size, output_length, output_size)
            "quantiles": quantiles,  # shape: (batch_size, output_length, output_size, len(quantiles))  # noqa: E501
        }

    def train(
        self,
        inputs_list: list[dict[str, Tensor]],
        epoch_count: int,
        learning_rate: float = 1e-3,
    ) -> dict[str, list[float]]:
        optimizer = Adam(params=self.parameters, lr=learning_rate)

        losses: list[float] = []

        for _ in range(epoch_count):
            epoch_loss: float = 0.0

            for inputs in inputs_list:
                outputs = self.forward(inputs)

                loss = quantile_loss(
                    outputs["quantiles"].reshape(
                        -1, self.output_size, len(self.quantiles)
                    ),
                    inputs["targets"].reshape(-1, self.output_size),
                    self.quantiles,
                )

                optimizer.zero_grad()
                _ = loss.backward()
                optimizer.step()

                epoch_loss += loss.numpy().item()

            average_epoch_loss: float = epoch_loss / len(inputs_list)
            losses.append(average_epoch_loss)

        return {
            "losses": losses,
        }

    def validate(
        self,
        inputs_list: list[dict[str, Tensor]],
    ) -> float:
        total_loss = 0.0
        batch_count = 0

        for inputs in inputs_list:
            outputs = self.forward(inputs)

            loss = quantile_loss(
                outputs["predictions"],
                inputs["targets"],
                self.quantiles,
            )

            total_loss += loss.numpy().item()
            batch_count += 1

        return total_loss / batch_count

    def save(
        self,
        path_and_file: str = "tft_model.safetensor",
    ) -> None:
        states = get_state_dict(self)
        safe_save(states, path_and_file)

    def load(
        self,
        path_and_file: str = "tft_model.safetensor",
    ) -> None:
        states = safe_load(path_and_file)
        load_state_dict(self, states)

    def predict(
        self,
        inputs: dict[str, Tensor],
    ) -> dict[str, Tensor]:
        outputs = self.forward(inputs)

        return {
            "predictions": outputs["predictions"],
            "quantiles": outputs["quantiles"],
        }
