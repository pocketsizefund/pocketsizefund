import polars as pl
from tinygrad import Tensor
from tinygrad.nn import Linear, LayerNorm
from typing import Dict
from copy import deepcopy


class TemporalFusionTransformer:
    def __init__(self) -> None:
        pass  # TEMP

    def train(
        self,
        data: pl.DataFrame,
    ) -> None:
        pass  # TEMP

    def predict(
        self,
        data: pl.DataFrame,
    ) -> any:  # TEMP (NEEDS TYPE)
        pass  # TEMP


class GatedResidualNetwork:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        output_size: int,
        dropout_rate: float = 0.1,
        context_size: int = None,  # NOTE: REMOVE (?)
    ) -> None:
        self.input_size = input_size
        self.output_size = output_size
        self.context_size = context_size
        self.hidden_size = hidden_size
        self.dropout_rate = dropout_rate

        self.fc1 = Linear(self.input_size, self.hidden_size)
        self.elu = Tensor.elu

        # NOTE: REMOVE (?)
        if self.context_size is not None:
            self.context = Linear(self.context_size, self.hidden_size, bias=False)

        self.fc2 = Linear(self.hidden_size, self.hidden_size)

        self.gate_norm = GateAddNorm(
            input_size=self.hidden_size,
            skip_size=self.output_size,
            hidden_size=self.output_size,
            dropout_rate=self.dropout_rate,
        )

    def forward(
        self,
        x: Tensor,
        context: Tensor = None,
    ) -> Tensor:
        residual = x

        x = self.fc1(x)
        if context is not None:
            x += self.context(context)
        x = self.elu(x)
        x = self.fc2(x)
        x = self.gate_norm(x, residual)

        return x


class VariableSelectionNetwork:
    def __init__(
        self,
        input_sizes: Dict[str, int],
        hidden_size: int,
        inputs_size_total: int,  # NOTE: ADD (?
        inputs_count: int = 1,  # NOTE: ADD (?
        input_embedding_flags: Dict[str, bool] = None,
        dropout_rate: float = 0.1,  # NOTE: CHANGE DEFAULT (?)
        context_size: int = None,  # NOTE: REMOVE (?)
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

        self.inputs_count = inputs_count

        if inputs_count > 1:
            if self.context_size is not None:
                self.flattend_grn = GatedResidualNetwork(
                    input_size=inputs_size_total,
                    hidden_size=min(hidden_size, inputs_count),
                    output_size=inputs_count,
                    dropout_rate=self.dropout_rate,
                    context_size=self.context_size,
                )
            else:
                self.flattend_grn = GatedResidualNetwork(
                    input_size=inputs_size_total,
                    hidden_size=min(hidden_size, inputs_count),
                    output_size=inputs_count,
                    dropout_rate=self.dropout_rate,
                )

        for name, input_size in input_sizes.items():
            if name in single_variable_grns:
                self.single_variable_grns[name] = single_variable_grns[name]

            # NOTE: ADD BACK RESAMPLE NORM (?)

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

        self.softmax = Tensor.softmax(axis=-1)

    def forward(
        self,
        x: Dict[str, Tensor],
        context: Tensor = None,
    ) -> Tensor:
        if self.inputs_count > 1:
            outputs = []
            weight_inputs = []
            for name in self.input_sizes.keys():
                variable_embedding = x[name]
                if name in self.prescalers:
                    variable_embedding = self.prescalers[name](variable_embedding)
                weight_inputs.append(variable_embedding)
                outputs.append(self.single_variable_grns[name](variable_embedding))
            outputs = Tensor.stack(outputs, dim=-1)

            flat_embedding = Tensor.cat(weight_inputs, dim=-1)
            sparse_weights = self.flattened_grn(flat_embedding, context)
            sparse_weights = self.softmax(sparse_weights).unsqueeze(-2)

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
                    outputs.size(0), outputs.size(1), 1, 1, device=outputs.device
                )
            else:
                sparse_weights = Tensor.ones(outputs.size(0), 1, 1, device=outputs.device)

        return outputs, sparse_weights


class GateAddNorm:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        skip_size: int,
        dropout_rate: float = None,
    ) -> None:
        self.input_size = input_size
        self.hidden_size = hidden_size or input_size
        self.skip_size = skip_size or self.hidden_size
        self.dropout_rate = dropout_rate

        self.glu = GatedLinearUnit(
            self.input_size,
            self.hidden_size,
            self.dropout_rate,
        )

        self.add_norm = AddNorm(
            self.hidden_size,
            self.skip_size,
        )

    def forward(
        self,
        x: Tensor,
        skip: Tensor,
    ) -> Tensor:
        x = self.glu(x)
        x = self.add_norm(x, skip)
        return x


class GatedLinearUnit:
    def __init__(
        self,
        input_size: int,
        output_size: int,
    ) -> None:
        self.weight_a = Tensor.uniform(input_size, output_size)
        self.bias_a = Tensor.zeros(output_size)
        self.weight_b = Tensor.uniform(input_size, output_size)
        self.bias_b = Tensor.zeros(output_size)

    def forward(
        self,
        x: Tensor,
        dropout_rate: float = None,
    ) -> Tensor:
        if dropout_rate is not None:
            x = Tensor.dropout(x, dropout_rate)

        x_a = x @ self.weight_a + self.bias_a
        x_b = x @ self.weight_b + self.bias_b

        return x_a * Tensor.sigmoid(x_b)


class AddNorm:
    def __init__(
        self,
        input_size: int,
        skip_size: int,
    ) -> None:
        self.input_size = input_size
        self.skip_size = skip_size

        # NOTE: DON'T INCLUDE RESAMPLE (?)

        self.norm = LayerNorm(self.input_size)

    def forward(
        self,
        x: Tensor,
        skip: Tensor,
    ) -> Tensor:
        return self.norm(x + skip)


# outline:
# [ ] components
# - [ ] static variable encoder
# - [ ] variable selection network
# - [ ] lstm encoder
# - [ ] lstm decoder
# - [x] add + norm w/ gate
# - [x] gated residual network
# - [ ] masked interpretable multi-head attention
# - [ ] dense network
# [ ] functions
# - [ ] init
# - - [ ] declare hyperparameters
# - - [ ] instantiate helper classes
# - [ ] train
# - - [ ] call helper classes
# - - [ ] generate model artifact
# - [ ] save model
# - - [ ] write artifact to path
# - [ ] load model
# - - [ ] read artifact from path
# - [ ] predict
# - - [ ] call helper classes
# - - [ ] return prediction
