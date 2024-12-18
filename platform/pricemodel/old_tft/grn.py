from tinygrad.tensor import Tensor
from tinygrad import nn
import numpy as np


class TimeDistributedInterpolation:
    def __init__(self, output_size: int, trainable: bool = False):
        self.output_size = output_size
        self.trainable = trainable

        if self.trainable:
            self.mask = Tensor.zeros(self.output_size, requires_grad=True)
            self.gate = nn.Sigmoid()

    def __call__(self, x):
        sample_size = x.size(1)

        # if len(x.size()) <= 2:
        #     return self.interpolate(x)
        #
        x_ = (
            x.contiguous()
            .view(-1, x.size(-1))
            .unsqueeze(1)
            .interpolate((self.output_size,), mode="linear", align_corners=True)
            .squeeze(1)
            .squeeze(1)
        )

        if self.trainable:
            x_ = x_ * self.gate(self.mask.unsqueeze(0)) * 2.0

        timestep_size = int(np.prod(x_.size()) // self.output_size // sample_size)

        return x_.reshape(timestep_size, sample_size, self.output_size)


class AddNorm:
    def __init__(self, input_size: int, skip_size: int | None = None, trainable_add: bool = True):
        self.input_size = input_size
        self.trainable_add = trainable_add
        self.skip_size = skip_size or input_size

        if self.input_size != self.skip_size:
            self.resample = TimeDistributedInterpolation(self.input_size, trainable=False)

        if self.trainable_add:
            self.mask = Tensor.zeros(self.input_size, requires_grad=True)
            self.gate = nn.Sigmoid()
        self.norm = nn.LayerNorm(self.input_size)

    def __call__(self, x: Tensor, skip: Tensor):
        if self.input_size != self.skip_size:
            skip = self.resample(skip)

        if self.trainable_add:
            skip = skip * self.gate(self.mask) * 2.0

        return self.norm(x + skip)


class GatedLinearUnit:
    def __init__(self, input_size: int, output_size, dropout: float = 0.0):
        self.dropout = dropout
        self.linear = nn.Linear(input_size, output_size)
        self.gate = nn.Linear(input_size, output_size)

    def __call__(self, x):
        linear = self.linear(x).dropout(self.dropout)
        gate = self.gate(x).sigmoid()
        return linear * gate


class GatedAddNorm:
    def __init__(
        self,
        input_size: int,
        output_size: int | None = None,
        skip_size: int | None = None,
        trainable_add: bool = False,
        dropout: float = 0.0,
    ):
        self.input_size = input_size
        self.output_size = output_size or input_size
        self.skip_size = skip_size or self.output_size
        self.dropout = dropout

        self.glu = GatedLinearUnit(
            self.input_size, output_size=self.output_size, dropout=self.dropout
        )
        self.add_norm = AddNorm(
            self.output_size, skip_size=self.skip_size, trainable_add=trainable_add
        )

    def __call__(self, x, skip):
        output = self.glu(x)
        output = self.add_norm(output, skip)
        return output


class ResampleNorm:
    def __init__(self, input_size: int, output_size: int | None = None, trainable_add: bool = True):
        self.input_size = input_size
        self.trainable_add = trainable_add
        self.output_size = output_size or input_size

        if self.input_size != self.output_size:
            self.resample = TimeDistributedInterpolation(self.output_size, trainable=False)

        if self.trainable_add:
            self.mask = Tensor.zeros(self.output_size, requires_grad=True)
            self.gate = nn.Sigmoid()

        self.norm = nn.LayerNorm(self.output_size)

    def __call__(self, x: Tensor) -> Tensor:
        if self.input_size != self.output_size:
            x = self.resample(x)

        if self.trainable_add:
            x = x * self.gate(self.mask) * 2.0

        return self.norm(x)


class GatedResidualNetwork:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        output_size: int,
        dropout: float = 0.1,
        context_size: int | None = None,
        residual: bool = False,
    ):
        self.input_size = input_size
        self.output_size = output_size
        self.context_size = context_size
        self.hidden_size = hidden_size
        self.dropout = dropout
        self.residual = residual

        if self.input_size != self.output_size and not self.residual:
            residual_size = self.input_size
        else:
            residual_size = self.output_size

        if self.output_size != residual_size:
            self.resample_norm = ResampleNorm(residual_size, self.output_size)

        self.fc1 = nn.Linear(self.input_size, self.hidden_size)

        if self.context_size is not None:
            self.context = nn.Linear(self.context_size, self.hidden_size, bias=False)

        self.fc2 = nn.Linear(self.hidden_size, self.hidden_size)
        self.init_weights()

        self.gate_norm = GatedAddNorm(
            input_size=self.hidden_size,
            skip_size=self.output_size,
            output_size=self.output_size,
            dropout=self.dropout,
            trainable_add=False,
        )

    def init_weights(self):
        for name, p in self.named_parameters():
            if "bias" in name:
                torch.nn.init.zeros_(p)
            elif "fc1" in name or "fc2" in name:
                torch.nn.init.kaiming_normal_(p, a=0, mode="fan_in", nonlinearity="leaky_relu")
            elif "context" in name:
                torch.nn.init.xavier_uniform_(p)

    def __call__(self, x, context=None, residual=None):
        if residual is None:
            residual = x

        if self.input_size != self.output_size and not self.residual:
            residual = self.resample_norm(residual)

        x = self.fc1(x)
        if context is not None:
            context = self.context(context)
            x = x + context
        x = x.elu()
        x = self.fc2(x)
        x = self.gate_norm(x, residual)
        return x
