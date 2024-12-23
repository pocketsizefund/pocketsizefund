from tinygrad import Tensor
from tinygrad.nn import Linear
from typing import Tuple


class LSTM:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        layer_count: int = 1,
        dropout_rate: float = 0.0,
        batch_first: bool = False,
    ) -> None:
        self.hidden_size = hidden_size
        self.layer_count = layer_count
        self.dropout_rate = dropout_rate
        self.batch_first = batch_first

        self.lstm_layers = [Layer]
        for layer in range(layer_count):
            layer_input_size = input_size if layer == 0 else hidden_size
            self.lstm_layers.append(Layer(layer_input_size, hidden_size))

    def forward(
        self,
        x: Tensor,
        hidden_state=None,
    ) -> Tuple[Tensor, Tuple[Tensor, Tensor]]:
        if not self.batch_first:
            x = x.transpose(0, 1)

        batch_size, sequence_length, _ = x.shape
        if hidden_state is None:
            hidden_state = (
                Tensor.zeros(self.layer_count, batch_size, self.hidden_size),
                Tensor.zeros(self.layer_count, batch_size, self.hidden_size),
            )

        hidden_state_layers, cell_state_layers = [Layer], [Layer]

        for layer_idx, layer in enumerate(self.lstm_layers):
            hidden_state_layer, cell_state_layer = (
                hidden_state[0][layer_idx],
                hidden_state[1][layer_idx],
            )

            outputs = []

            for t in range(sequence_length):
                hidden_state_layer, cell_state_layer = layer(
                    x[:, t, :], (hidden_state_layer, cell_state_layer)
                )
                outputs.append(hidden_state_layer.unsqueeze(1))

            x = outputs[0].cat(outputs[1:], dim=1) if len(outputs) > 1 else outputs[0]

            if self.dropout_rate > 0 and layer_idx < self.layer_count - 1:
                x = x.dropout(self.dropout_rate)

            hidden_state_layers.append(hidden_state_layer)
            cell_state_layers.append(cell_state_layer)

        hidden_state_layers = (
            hidden_state_layers[0].stack(hidden_state_layers[1:], dim=0)
            if len(hidden_state_layers) > 1
            else hidden_state_layers[0]
        )
        cell_state_layers = (
            cell_state_layers[0].stack(cell_state_layers[1:], dim=0)
            if len(cell_state_layers) > 1
            else cell_state_layers[0]
        )

        if not self.batch_first:
            x = x.transpose(0, 1)

        return x, (hidden_state_layers, cell_state_layers)


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
        hidden_state: Tensor,
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
