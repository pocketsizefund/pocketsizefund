from tinygrad import Tensor
from tinygrad.nn import LayerNorm
from interpolation import TimeDistributedInterpolation


class ResampleNorm:
    def __init__(
        self,
        input_size: int,
        output_size: int,
    ) -> None:
        self.input_size = input_size
        self.output_size = output_size or input_size

        if self.input_size != self.output_size:
            self.resample = TimeDistributedInterpolation(self.output_size)

        self.norm = LayerNorm(self.output_size)

    def forward(self, x: Tensor) -> Tensor:
        if self.input_size != self.output_size:
            x = self.resample(x)

        return self.norm(x)
