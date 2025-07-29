from tinygrad import nn
from tinygrad.tensor import Tensor


class VariableSelectionNetwork:
    def __init__(self, input_dimension: int, hidden_size: int) -> None:
        self.input_layer = nn.Linear(
            in_features=input_dimension,
            out_features=hidden_size,
        )

        self.output_layer = nn.Linear(
            in_features=hidden_size,
            out_features=input_dimension,
        )

    def forward(self, inputs: Tensor) -> Tensor:
        inputs = self.input_layer(inputs)
        inputs = inputs.relu()
        inputs = self.output_layer(inputs)
        return inputs.sigmoid()
