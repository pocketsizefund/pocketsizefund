from typing import Dict
from tinygrad import Tensor
from tinygrad.nn import Linear
from copy import deepcopy
from resample import ResampleNorm
from gated_residual_network import GatedResidualNetwork
from typing import Optional, Tuple


class VariableSelectionNetwork:
    def __init__(
        self,
        input_sizes: Dict[str, int],
        hidden_size: int,
        input_embedding_flags: Dict[str, bool] = None,
        dropout_rate: float = 0.1,
        context_size: int = None,
        single_variable_grns: Dict[str, GatedResidualNetwork] = {},  # NOTE: CHANGE TYPE (?)
        prescalers: Dict[str, Linear] = {},
    ) -> None:
        self.input_sizes = input_sizes
        self.hidden_size = hidden_size

        self.input_embedding_flags = input_embedding_flags
        self._input_embedding_flags = (
            {} if input_embedding_flags is None else deepcopy(input_embedding_flags)
        )

        self.dropout_rate = dropout_rate
        self.context_size = context_size

        if len(input_sizes) > 1:
            if self.context_size is not None:
                self.flattened_grn = GatedResidualNetwork(
                    self.input_size_total,  # NOTE: FIX
                    min(self.hidden_size, len(input_sizes)),
                    len(input_size),
                    self.dropout_rate,
                    self.context_size,
                    residual=False,
                )
            else:
                self.flattened_grn = GatedResidualNetwork(
                    self.input_size_total,  # NOTE: FIX
                    min(self.hidden_size, len(input_sizes)),
                    len(input_sizes),
                    self.dropout_rate,
                    residual=False,
                )

        for name, input_size in input_sizes.items():
            if name in single_variable_grns:
                self.single_variable_grns[name] = single_variable_grns[name]

            elif self._input_embedding_flags.get(name, False):
                self.single_variable_grns[name] = ResampleNorm(input_size, self.hidden_size)

            else:
                self.single_variable_grns[name] = GatedResidualNetwork(
                    input_size=input_size,
                    hidden_size=min(self.input_size, self.hidden_size),
                    output_size=self.hidden_size,
                    dropout_rate=self.dropout_rate,
                )

            if name in prescalers:
                self.prescalers[name] = prescalers[name]
            elif not self._input_embedding_flags.get(name, False):
                self.prescalers[name] = Linear(input_size, hidden_size)

    def forward(
        self,
        x: Dict[str, Tensor],
        context: Tensor = None,
    ) -> Tuple[Tensor, Tensor]:
        if self.inputs_count > 1:
            outputs = []
            weight_inputs = []
            for name in self.input_sizes.keys():
                variable_embedding = x[name]
                if name in self.prescalers:
                    variable_embedding = self.prescalers[name](variable_embedding)
                weight_inputs.append(variable_embedding)
                outputs.append(self.single_variable_grns[name](variable_embedding))

            flat_embedding = None
            if len(outputs) > 1:
                first_output = outputs[0]
                outputs = first_output.stack(outputs[1:], dim=-1)
                first_weight_input = weight_inputs[0]
                flat_embedding = first_weight_input.stack(weight_inputs[1:], dim=-1)
            else:
                outputs = outputs[0]
                flat_embedding = weight_inputs[0]

            sparse_weights = self.flattened_grn(flat_embedding, context)
            sparse_weights = sparse_weights.softmax().unsqueeze(-2)

            outputs = outputs * sparse_weights
            outputs = outputs.sum(dim=-1)

        else:
            name = next(iter(self.input_sizes.keys()))
            variable_embedding = x[name]

            if name in self.prescalers:
                variable_embedding = self.prescalers[name](variable_embedding)

            outputs = self.single_variable_grns[name](variable_embedding)
            if outputs.ndim == 3:
                sparse_weights = Tensor.ones(
                    outputs.size(0),
                    outputs.size(1),
                    1,
                    1,
                    device=outputs.device,
                )
            else:
                sparse_weights = Tensor.ones(outputs.size(0), 1, 1, device=outputs.device)

        return outputs, sparse_weights
