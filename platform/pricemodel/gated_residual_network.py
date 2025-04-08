from tinygrad import Tensor
from tinygrad.nn import Linear, LayerNorm


class GatedResidualNetwork:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        output_size: int,
    ) -> None:
        output_size = output_size if output_size is not None else input_size

        self.dense_input = Linear(in_features=input_size, out_features=hidden_size)
        self.dense_output = Linear(in_features=hidden_size, out_features=output_size)
        self.gate = Linear(in_features=hidden_size, out_features=output_size)
        self.layer_normalizer = LayerNorm(normalized_shape=output_size)

    def forward(self, input: Tensor) -> Tensor:
        hidden_state = self.dense_input(input).relu()

        output_state = self.dense_output(hidden_state)

        gate_state = self.gate(hidden_state).sigmoid()

        output = self.layer_normalizer(gate_state * output_state + input)

        return output
