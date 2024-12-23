from tinygrad import Tensor
from tinygrad.nn import Linear, LayerNorm
from interpolation import TimeDistributedInterpolation


class GatedResidualNetwork:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        output_size: int,
        dropout_rate: float = 0.1,
        context_size: int = None,
    ) -> None:
        self.input_size = input_size
        self.hidden_size = hidden_size
        self.output_size = output_size
        self.dropout_rate = dropout_rate
        self.context_size = context_size

        self.fc1 = Linear(self.input_size, self.hidden_size)

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

        x = x.elu(x)
        x = self.fc2(x)
        x = self.gate_norm(x, residual)

        return x


class GateAddNorm:
    def __init__(
        self,
        input_size: int,
        hidden_size: int = None,
        skip_size: int = None,
        dropout_rate: float = None,
    ) -> None:
        hidden_size = hidden_size or input_size
        skip_size = skip_size or hidden_size

        self.glu = GatedLinearUnit(
            input_size=input_size,
            hidden_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        self.add_norm = AddNorm(
            input_size=hidden_size,
            skip_size=skip_size,
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
        hidden_size: int = None,
        dropout_rate: float = None,
    ) -> None:
        self.dropout_rate = dropout_rate

        hidden_size = hidden_size or input_size

        self.fc = Linear(input_size, hidden_size * 2)

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        if self.dropout_rate is not None:
            x = x.dropout(self.dropout_rate)

        x = self.fc(x)

        split_dimension = -1

        split_size = x.shape[split_dimension] // 2
        x1, x2 = input.split(split_size, axis=split_dimension)

        gate = x2.sigmoid()

        return x1 * gate


class AddNorm:
    def __init__(
        self,
        input_size: int,
        skip_size: int = None,
    ) -> None:
        self.input_size = input_size
        self.skip_size = skip_size or input_size

        if self.input_size != self.skip_size:
            self.resample = TimeDistributedInterpolation(self.input_size)

        self.norm = LayerNorm(self.input_size)

    def forward(
        self,
        x: Tensor,
        skip: Tensor,
    ) -> Tensor:
        if self.input_size != self.skip_size:
            skip = self.resample(skip)

        return self.norm(x + skip)
