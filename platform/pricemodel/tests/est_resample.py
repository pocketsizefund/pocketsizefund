from tinygrad import Tensor
from pricemodel.resample import ResampleNorm


def test_resample_norm():
    input_size = 3
    output_size = 5

    class MockLayerNorm:
        def __init__(self, size):
            self.size = size

        def __call__(self, x: Tensor) -> Tensor:
            return x - x.mean(axis=-1, keepdims=True)  # Mock normalization

    class MockTimeDistributedInterpolation:
        def __init__(self, size):
            self.size = size

        def forward(self, x: Tensor) -> Tensor:
            return Tensor.linspace(0, 1, self.size).unsqueeze(0).repeat(x.shape[0], 1)

    ResampleNorm.TimeDistributedInterpolation = MockTimeDistributedInterpolation
    ResampleNorm.LayerNorm = MockLayerNorm

    resample_norm = ResampleNorm(input_size=input_size, output_size=output_size)

    assert resample_norm.input_size == input_size, "Incorrect input size initialization"
    assert resample_norm.output_size == output_size, "Incorrect output size initialization"

    input_tensor = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
    output_tensor = resample_norm.forward(input_tensor)
    assert output_tensor.shape == (
        2,
        output_size,
    ), "Incorrect output shape after resampling and normalization"

    resample_norm_no_resample = ResampleNorm(input_size=input_size, output_size=input_size)
    output_tensor_no_resample = resample_norm_no_resample.forward(input_tensor)
    assert output_tensor_no_resample.shape == (
        2,
        input_size,
    ), "Incorrect output shape without resampling"
    assert (
        (output_tensor_no_resample.mean(axis=-1).abs() < 1e-5).all().item()
    ), "Normalization did not zero mean"
