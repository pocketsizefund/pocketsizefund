from tinygrad import Tensor
from tinygrad.nn import Linear
from typing import Tuple, List


class LSTM:
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

        self.lstm_layers: List[Layer] = []
        for layer in range(layer_count):
            layer_input_size = input_size if layer == 0 else hidden_size
            self.lstm_layers.append(Layer(layer_input_size, hidden_size))

    def forward(
        self,
        x: Tensor,
        hidden_state: Tuple[Tensor, Tensor] = None,
    ) -> Tuple[Tensor, Tuple[Tensor, Tensor]]:
        batch_size, sequence_length, _ = x.shape
        if hidden_state is None:
            hidden_state = (
                Tensor.zeros(self.layer_count, batch_size, self.hidden_size),
                Tensor.zeros(self.layer_count, batch_size, self.hidden_size),
            )

        hidden_state_layers: List[Tensor] = []
        cell_state_layers: List[Tensor] = []

        hidden_state_output = Tensor.empty(batch_size, self.hidden_size)
        cell_state_output = Tensor.empty(batch_size, self.hidden_size)

        for layer_idx, layer in enumerate(self.lstm_layers):
            hidden_state_layer, cell_state_layer = (
                hidden_state[0][layer_idx],
                hidden_state[1][layer_idx],
            )

            outputs: Tensor = Tensor.empty(batch_size, sequence_length, self.hidden_size)

            for t in range(sequence_length):
                hidden_state_layer, cell_state_layer = layer.forward(
                    x=x[:, t, :],
                    hidden_state=(hidden_state_layer, cell_state_layer),
                )
                outputs[:, t, :] = hidden_state_layer

            x = outputs

            if self.dropout_rate > 0 and layer_idx < self.layer_count - 1:
                x = x.dropout(self.dropout_rate)

            hidden_state_layers.append(hidden_state_layer)
            cell_state_layers.append(cell_state_layer)

        hidden_state_output = hidden_state_layers[0].stack(*hidden_state_layers[1:], dim=0)
        cell_state_output = cell_state_layers[0].stack(*cell_state_layers[1:], dim=0)

        return x, (hidden_state_output, cell_state_output)


class Layer:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
    ) -> None:
        self.input_weight = Linear(input_size, 4 * hidden_size, bias=True)
        self.hidden_weight = Linear(hidden_size, 4 * hidden_size, bias=False)

    def forward(
        self,
        x: Tensor,
        hidden_state: Tuple[Tensor, Tensor],
    ) -> Tuple[Tensor, Tensor]:
        hidden_previous_state, cell_previous_state = hidden_state

        gates = self.input_weight(x) + self.hidden_weight(hidden_previous_state)

        input_gate, forget_gate, candidate_gate, output_gate = gates.chunk(4, dim=-1)

        input_gate = input_gate.sigmoid()
        forget_gate = forget_gate.sigmoid()
        candidate_gate = candidate_gate.tanh()
        output_gate = output_gate.sigmoid()

        cell_current_state = forget_gate * cell_previous_state + input_gate * candidate_gate
        hidden_current_state = output_gate * cell_current_state.tanh()

        return hidden_current_state, cell_current_state
