from typing import List, Tuple
from tinygrad import Tensor
from tinygrad.nn import LSTMCell


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

        self.layers: List[LSTMCell] = []
        for index in range(layer_count):
            input_size = input_size if index == 0 else self.hidden_size
            self.layers.append(LSTMCell(input_size, self.hidden_size))

    def forward(
        self,
        input: Tensor,
    ) -> Tuple[Tensor, Tuple[Tensor, Tensor]]:
        batch_size, sequence_length, _ = input.shape

        hidden_states = [
            Tensor.zeros(batch_size, self.hidden_size) for _ in range(self.layer_count)
        ]
        cell_states = [Tensor.zeros(batch_size, self.hidden_size) for _ in range(self.layer_count)]

        output_states: List[Tensor] = []
        for t in range(sequence_length):
            layer_input = input[:, t]

            for index, layer in enumerate(self.layers):
                layer_hidden_state, layer_cell_state = layer(
                    x=layer_input,
                    hc=(
                        hidden_states[index],
                        cell_states[index],
                    ),
                )

                layer_hidden_state = layer_hidden_state.realize()  # keep realize
                layer_cell_state = layer_cell_state.realize()  # keep realize

                if self.dropout_rate > 0.0 and index < self.layer_count - 1:
                    layer_hidden_state.train()
                    layer_hidden_state = layer_hidden_state.dropout(self.dropout_rate)

                hidden_states[index] = layer_hidden_state
                cell_states[index] = layer_cell_state

                layer_input = layer_hidden_state

            output_states.append(layer_hidden_state)

        output_state = output_states[0].stack(*output_states[1:], dim=1).realize()  # keep realize

        last_hidden_state = hidden_states[-1].realize()  # keep realize
        last_cell_state = cell_states[-1].realize()  # keep realize

        return output_state, (last_hidden_state, last_cell_state)
