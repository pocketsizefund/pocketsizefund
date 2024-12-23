from tinygrad import Tensor


class TimeDistributedInterpolation:
    def __init__(
        self,
        output_size: int,
    ) -> None:
        self.output_size = output_size

    def interpolate(
        self,
        x: Tensor,
    ) -> Tensor:
        upsampled = x.interpolate(
            x.unsqueeze(1),
            self.output_size,
            mode="linear",
            align_corners=True,
        ).squeeze(1)

        return upsampled

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        if len(x.size()) <= 2:
            return self.interpolate(x)

        x_reshape = x.contiguous().view(-1, x.size(-1))

        y = self.interpolate(x_reshape)
        y = y.contiguous().view(x.size(0), -1, y.size(-1))

        return y
