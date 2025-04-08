from tinygrad import Tensor
from tinygrad.nn import Linear


class ProjectionLayer:
    def __init__(
        self,
        input_size: int,
        output_size: int,
    ) -> None:
        self.linear = Linear(input_size, output_size)

    def forward(self, input: Tensor) -> Tensor:
        x = self.linear(input)
        batch_size = x.shape[0]
        output = x.mean(axis=1).reshape(batch_size, 5, 3)

        return output
