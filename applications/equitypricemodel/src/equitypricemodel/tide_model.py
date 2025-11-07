import json
import os
from typing import cast

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


def quantile_loss(
    predictions: Tensor,
    targets: Tensor,
    quantiles: list[float] | None = None,
) -> Tensor:
    if quantiles is None:
        quantiles = [0.1, 0.5, 0.9]

    if not all(0 <= q <= 1 for q in quantiles):
        message = "All quantiles must be between 0 and 1"
        raise ValueError(message)

    errors_total = Tensor(0.0)
    for index, quantile in enumerate(quantiles):
        error = targets.sub(predictions[:, :, index])
        quantile_tensor = Tensor(quantile)
        errors_total = errors_total.add(
            Tensor.where(
                error > 0,
                cast("Tensor", quantile_tensor.mul(error)),
                cast("Tensor", (quantile_tensor.sub(1)).mul(error)),
            ).mean()
        )

    return cast("Tensor", errors_total.div(len(quantiles)))


class _ResidualBlock:
    """Residual block with layer normalization and dropout"""

    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        dropout_rate: float = 0.1,
    ) -> None:
        self.input_size = input_size
        self.hidden_size = hidden_size
        self.dropout_rate = dropout_rate

        self.dense = Linear(in_features=input_size, out_features=hidden_size)

        self.skip_connection = None
        if input_size != hidden_size:
            self.skip_connection = Linear(
                in_features=input_size,
                out_features=hidden_size,
            )

    def forward(self, x: Tensor) -> Tensor:
        x = x.cast("float32")  # ensure float32 precision

        out = self.dense(x).relu()  # relu activation

        if Tensor.training and self.dropout_rate > 0:
            out = out.dropout(p=self.dropout_rate)

        skip = x
        if self.skip_connection is not None:
            skip = self.skip_connection(x)

        out = cast("Tensor", out.add(skip))  # add residual connection

        mean = out.mean(axis=-1, keepdim=True)
        var = ((out.sub(mean)) ** 2).mean(axis=-1, keepdim=True)
        return cast(
            "Tensor",
            (out - mean) / (var + Tensor(1e-5).cast("float32")).sqrt(),
        )


class Model:
    """
    TiDE architecture for time series forecasting

    Model paper reference: https://arxiv.org/pdf/2304.08424"""

    def __init__(  # noqa: PLR0913
        self,
        input_size: int,
        hidden_size: int = 128,
        num_encoder_layers: int = 2,
        num_decoder_layers: int = 2,
        output_length: int = 7,
        dropout_rate: float = 0.1,
        quantiles: list[float] | None = None,
    ) -> None:
        self.input_size = input_size
        self.hidden_size = hidden_size
        self.num_encoder_layers = num_encoder_layers
        self.num_decoder_layers = num_decoder_layers
        self.output_length = output_length
        self.dropout_rate = dropout_rate
        self.quantiles = quantiles or [0.1, 0.5, 0.9]

        self.feature_projection = Linear(
            in_features=self.input_size,
            out_features=self.hidden_size,
        )

        self.encoder_blocks: list[_ResidualBlock] = []
        for _ in range(self.num_encoder_layers):
            block_input_size = self.hidden_size
            self.encoder_blocks.append(
                _ResidualBlock(
                    input_size=block_input_size,
                    hidden_size=self.hidden_size,
                    dropout_rate=self.dropout_rate,
                )
            )

        self.decoder_blocks: list[_ResidualBlock] = []
        for _ in range(self.num_decoder_layers):
            block_input_size = self.hidden_size
            self.decoder_blocks.append(
                _ResidualBlock(
                    input_size=block_input_size,
                    hidden_size=self.hidden_size,
                    dropout_rate=self.dropout_rate,
                )
            )

        self.temporal_projection = Linear(  # projects to output sequence length
            in_features=self.hidden_size,
            out_features=self.hidden_size * self.output_length,
        )

        # final output layer for quantiles
        self.output_layer = Linear(
            in_features=self.hidden_size,
            out_features=len(self.quantiles),
        )

    def forward(self, x: Tensor) -> Tensor:
        """
        Forward pass through TiDE model
        Args:
            x: Input tensor of shape (batch_size, flattened_features)
        Returns:
            Tensor of shape (batch_size, output_length, num_quantiles)
        """
        x = x.cast("float32")  # ensure float32 precision
        batch_size = x.shape[0]

        x = self.feature_projection(x).relu()

        for encoder_block in self.encoder_blocks:
            x = encoder_block.forward(x)

        encoder_output = cast("Tensor", x)

        for decoder_block in self.decoder_blocks:
            x = decoder_block.forward(x)

        # add skip connection from encoder to decoder output
        x = cast("Tensor", x.add(encoder_output))

        # temporal projection to output sequence length
        x = self.temporal_projection(x).relu()

        # reshape to (batch_size, output_length, hidden_size)
        x = x.reshape(batch_size, self.output_length, self.hidden_size)

        # apply output layer across the sequence dimension
        predictions: list[Tensor] = []
        for t in range(self.output_length):
            pred_t = self.output_layer(x[:, t, :])  # (batch_size, num_quantiles)
            predictions.append(pred_t)

        predictions_first = predictions[0]
        predictions_rest = predictions[1:]

        # stack predictions: (batch_size, output_length, num_quantiles e.g. (32, 7, 3))
        return predictions_first.stack(*predictions_rest, dim=1)

    def train(
        self,
        train_batches: list,
        epochs: int = 10,
        learning_rate: float = 0.001,
    ) -> list:
        """Train the TiDE model using quantile loss"""
        Tensor.training = True

        parameters = get_parameters(self)
        optimizer = Adam(params=parameters, lr=learning_rate)
        losses = []

        for _ in range(epochs):
            epoch_losses = []

            for batch in train_batches:
                combined_input_features, targets, batch_size = (
                    self._combine_input_features(batch)
                )

                # predictions shape: (batch_size, output_length, num_quantiles)
                predictions = self.forward(combined_input_features)

                # reshape targets to (batch_size, output_length)
                targets_reshaped = targets.reshape(batch_size, self.output_length)

                loss = quantile_loss(predictions, targets_reshaped, self.quantiles)

                optimizer.zero_grad()
                loss.backward()
                optimizer.step()

                epoch_losses.append(loss.numpy().item())

            avg_loss = sum(epoch_losses) / len(epoch_losses)
            losses.append(avg_loss)

        return losses

    def validate(self, validation_batches: list) -> float:
        """Validate the model using quantile loss"""
        Tensor.training = False
        validation_losses = []

        for batch in validation_batches:
            combined_input, targets, batch_size = self._combine_input_features(batch)

            predictions = self.forward(combined_input)

            targets_reshaped = targets.reshape(batch_size, self.output_length)

            loss = quantile_loss(predictions, targets_reshaped, self.quantiles)
            validation_losses.append(loss.numpy().item())

        return sum(validation_losses) / len(validation_losses)

    def save(
        self,
        directory_name: str = "",
        parameters_file_name: str = "tide_parameters.json",
        states_file_name: str = "tide_states.safetensor",
    ) -> None:
        os.makedirs(os.path.dirname(directory_name), exist_ok=True)  # noqa: PTH120, PTH103

        states = get_state_dict(self)

        safe_save(states, os.path.join(directory_name, states_file_name))  # noqa: PTH118

        parameters = {
            "input_size": self.input_size,
            "hidden_size": self.hidden_size,
            "num_encoder_layers": self.num_encoder_layers,
            "num_decoder_layers": self.num_decoder_layers,
            "output_length": self.output_length,
            "dropout_rate": self.dropout_rate,
            "quantiles": self.quantiles,
        }

        with open(  # noqa: PTH123
            os.path.join(directory_name, parameters_file_name),  # noqa: PTH118
            "w",
        ) as parameters_file:
            json.dump(parameters, parameters_file)

    @classmethod
    def load(
        cls,
        directory_name: str = "",
        parameters_file_name: str = "tide_parameters.json",
        states_file_name: str = "tide_states.safetensor",
    ) -> "Model":
        states = safe_load(os.path.join(directory_name, states_file_name))  # noqa: PTH118
        with open(  # noqa: PTH123
            os.path.join(directory_name, parameters_file_name)  # noqa: PTH118
        ) as parameters_file:
            parameters = json.load(parameters_file)

        model = cls(**parameters)

        load_state_dict(model, states)

        return model

    def predict(
        self,
        inputs: dict[str, Tensor],
    ) -> Tensor:
        combined_input_features, _, _ = self._combine_input_features(inputs)

        # outputs shape: (batch_size, output_length, num_quantiles)
        return self.forward(combined_input_features)

    def _combine_input_features(
        self, x: dict[str, Tensor]
    ) -> tuple[Tensor, Tensor, int]:
        batch_size = x["encoder_continuous_features"].shape[0]

        encoder_cont_flat = x["encoder_continuous_features"].reshape(batch_size, -1)
        encoder_cat_flat = (
            x["encoder_categorical_features"].reshape(batch_size, -1).cast("float32")
        )
        decoder_cat_flat = (
            x["decoder_categorical_features"].reshape(batch_size, -1).cast("float32")
        )
        static_cat_flat = (
            x["static_categorical_features"].reshape(batch_size, -1).cast("float32")
        )

        return (
            Tensor.cat(
                encoder_cont_flat,
                encoder_cat_flat,
                decoder_cat_flat,
                static_cat_flat,
                dim=1,
            ),
            x["targets"],
            int(batch_size),
        )
