from tinygrad.nn import LSTMCell
from tinygrad.tensor import Tensor


class LongShortTermMemory:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        layer_count: int = 1,
        dropout_rate: float = 0.0,
    ) -> None:
        self.hidden_size = hidden_size
        self.layer_count = layer_count
        self.dropout_rate = dropout_rate

        self.layers: list[LSTMCell] = []
        for index in range(layer_count):
            input_size = input_size if index == 0 else self.hidden_size
            self.layers.append(LSTMCell(input_size, self.hidden_size))

    def forward(
        self,
        features: Tensor,
    ) -> tuple[Tensor, tuple[Tensor, Tensor]]:
        batch_size, sequence_length, _ = features.shape

        hidden_state = Tensor.zeros(
            self.layer_count, batch_size, self.hidden_size
        ).contiguous()
        cell_state = Tensor.zeros(
            self.layer_count, batch_size, self.hidden_size
        ).contiguous()

        outputs = []

        for t in range(int(sequence_length)):
            layer_input = features[:, t]

            for index, layer in enumerate(self.layers):
                layer_hidden_state, layer_cell_state = layer(
                    x=layer_input,
                    hc=(
                        hidden_state[index],
                        cell_state[index],
                    ),
                )

                hidden_state[index] = layer_hidden_state
                cell_state[index] = layer_cell_state

                if self.dropout_rate > 0.0 and index < self.layer_count - 1:
                    hidden_state[index].train()
                    hidden_state[index] = hidden_state[index].dropout(self.dropout_rate)

                layer_input = layer_hidden_state

            outputs.append(hidden_state[-1])

        if not outputs:
            message = "Cannot stack empty outputs list"
            raise ValueError(message)

        if len(outputs) == 1:
            output_tensor = outputs[0].unsqueeze(1)
        else:
            output_tensor = Tensor.stack(outputs[0], *outputs[1:], dim=1)

        return output_tensor, (hidden_state, cell_state)
