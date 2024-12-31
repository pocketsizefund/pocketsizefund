from typing import Dict, Tuple
from tinygrad import Tensor
from tinygrad.nn import Linear
from copy import deepcopy
from pricemodel.resample import ResampleNorm
from pricemodel.gated_residual_network import GatedResidualNetwork


class VariableSelectionNetwork:
    def __init__(
        self,
        input_sizes: Dict[str, int],
        hidden_size: int,
        input_embedding_flags: Dict[str, bool] = None,
        dropout_rate: float = 0.1,
        context_size: int = None,
        single_variable_grns: Dict[str, GatedResidualNetwork] = {},
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
        self.single_variable_grns = single_variable_grns
        self.prescalers = prescalers

        if self.input_count > 1:
            if self.context_size is not None:
                self.flattened_grn = GatedResidualNetwork(
                    self.input_size_total,
                    min(self.hidden_size, self.input_count),
                    self.input_count,
                    self.dropout_rate,
                    self.context_size,
                )
            else:
                self.flattened_grn = GatedResidualNetwork(
                    self.input_size_total,
                    (self.hidden_size, self.input_count),
                    self.input_count,
                    self.dropout_rate,
                )

        for name, input_size in input_sizes.items():
            if name in single_variable_grns:
                self.single_variable_grns[name] = single_variable_grns[name]

            elif self._input_embedding_flags.get(name, False):
                self.single_variable_grns[name] = ResampleNorm(input_size, self.hidden_size)

            else:
                self.single_variable_grns[name] = GatedResidualNetwork(
                    input_size=input_size,
                    hidden_size=min(self.input_count, self.hidden_size),
                    output_size=self.hidden_size,
                    dropout_rate=self.dropout_rate,
                )

            if name in prescalers:
                self.prescalers[name] = prescalers[name]
            elif not self._input_embedding_flags.get(name, False):
                self.prescalers[name] = Linear(input_size, self.hidden_size)
            else:
                self.prescalers[name] = Linear(input_size, self.hidden_size)

    @property
    def input_size_total(self):
        return sum(
            size if name in self._input_embedding_flags else size
            for name, size in self.input_sizes.items()
        )

    @property
    def input_count(self):
        return len(self.input_sizes)

    def forward(
        self,
        x: Dict[str, Tensor],
        context: Tensor = None,
    ) -> Tuple[Tensor, Tensor]:
        if self.input_count > 1:
            outputs = []
            weight_inputs = []
            for name in self.input_sizes.keys():
                variable_embedding = x[name]

                if name in self.prescalers:
                    variable_embedding = self.prescalers[name](variable_embedding)

                weight_inputs.append(variable_embedding)
                outputs.append(self.single_variable_grns[name].forward(variable_embedding))

            flat_embedding = None
            if len(outputs) > 1:
                first_output = outputs[0]
                outputs = first_output.stack(outputs[1:], dim=-1)
                first_weight_input = weight_inputs[0]
                flat_embedding = first_weight_input.stack(weight_inputs[1:], dim=-1)
            else:
                outputs = outputs[0]
                flat_embedding = weight_inputs[0]

            sparse_weights = self.flattened_grn.forward(flat_embedding, context)
            sparse_weights = sparse_weights.softmax().unsqueeze(-2)

            outputs = outputs * sparse_weights
            outputs = outputs.sum(dim=-1)

        else:
            name = next(iter(self.input_sizes.keys()))
            variable_embedding = x[name]

            if name in self.prescalers:
                variable_embedding = self.prescalers[name](variable_embedding)

            outputs = self.single_variable_grns[name].forward(variable_embedding)
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
