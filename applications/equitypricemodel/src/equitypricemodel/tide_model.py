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
            in_features=input_size,
            out_features=hidden_size,
        )

        self.encoder_blocks: list[_ResidualBlock] = []
        for _ in range(num_encoder_layers):
            block_input_size = hidden_size
            self.encoder_blocks.append(
                _ResidualBlock(
                    input_size=block_input_size,
                    hidden_size=hidden_size,
                    dropout_rate=dropout_rate,
                )
            )

        self.decoder_blocks: list[_ResidualBlock] = []
        for _ in range(num_decoder_layers):
            block_input_size = hidden_size
            self.decoder_blocks.append(
                _ResidualBlock(
                    input_size=block_input_size,
                    hidden_size=hidden_size,
                    dropout_rate=dropout_rate,
                )
            )

        self.temporal_projection = Linear(  # projects to output sequence length
            in_features=hidden_size,
            out_features=hidden_size * output_length,
        )

        # final output layer for quantiles
        self.output_layer = Linear(
            in_features=hidden_size,
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
                # Combine all input features
                encoder_continuous = batch["encoder_continuous_features"]
                encoder_categorical = batch["encoder_categorical_features"]
                decoder_categorical = batch["decoder_categorical_features"]
                static_categorical = batch["static_categorical_features"]
                targets = batch["targets"]

                batch_size = encoder_continuous.shape[0]

                # flatten and concatenate all features
                encoder_cont_flat = encoder_continuous.reshape(batch_size, -1)
                encoder_cat_flat = encoder_categorical.reshape(batch_size, -1).cast(
                    "float32"
                )
                decoder_cat_flat = decoder_categorical.reshape(batch_size, -1).cast(
                    "float32"
                )
                static_cat_flat = static_categorical.reshape(batch_size, -1).cast(
                    "float32"
                )

                combined_input = Tensor.cat(
                    encoder_cont_flat,
                    encoder_cat_flat,
                    decoder_cat_flat,
                    static_cat_flat,
                    dim=1,
                )

                # predictions shape: (batch_size, output_length, num_quantiles)
                predictions = self.forward(combined_input)

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
            encoder_continuous = batch["encoder_continuous_features"]
            encoder_categorical = batch["encoder_categorical_features"]
            decoder_categorical = batch["decoder_categorical_features"]
            static_categorical = batch["static_categorical_features"]
            targets = batch["targets"]

            batch_size = encoder_continuous.shape[0]

            encoder_cont_flat = encoder_continuous.reshape(batch_size, -1)
            encoder_cat_flat = encoder_categorical.reshape(batch_size, -1).cast(
                "float32"
            )
            decoder_cat_flat = decoder_categorical.reshape(batch_size, -1).cast(
                "float32"
            )
            static_cat_flat = static_categorical.reshape(batch_size, -1).cast("float32")

            combined_input = Tensor.cat(
                encoder_cont_flat,
                encoder_cat_flat,
                decoder_cat_flat,
                static_cat_flat,
                dim=1,
            )

            predictions = self.forward(combined_input)

            targets_reshaped = targets.reshape(batch_size, self.output_length)

            loss = quantile_loss(predictions, targets_reshaped, self.quantiles)
            validation_losses.append(loss.numpy().item())

        return sum(validation_losses) / len(validation_losses)

    def save(
        self,
        path_and_file: str = "tft_model.safetensor",
    ) -> None:
        directory = os.path.dirname(path_and_file)  # noqa: PTH120

        os.makedirs(os.path.dirname(directory), exist_ok=True)  # noqa: PTH120, PTH103

        states = get_state_dict(self)

        safe_save(states, path_and_file)

    def load(
        self,
        path_and_file: str = "tft_model.safetensor",
    ) -> None:
        states = safe_load(path_and_file)
        load_state_dict(self, states)
