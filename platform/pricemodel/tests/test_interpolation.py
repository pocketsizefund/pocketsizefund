from pricemodel.interpolation import TimeDistributedInterpolation
from tinygrad import Tensor


def test_time_distributed_interpolation():
    output_size = 5
    interpolator = TimeDistributedInterpolation(output_size=output_size)

    input_tensor_2d = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
    output_tensor_2d = interpolator.forward(input_tensor_2d)
    assert output_tensor_2d.shape == (2, output_size), "Incorrect shape for 2D forward pass"

    input_tensor_3d = Tensor([[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]])
    output_tensor_3d = interpolator.forward(input_tensor_3d)
    assert output_tensor_3d.shape == (1, 2, output_size), "Incorrect shape for 3D forward pass"

    assert (
        output_tensor_3d[:, :, 1:] - output_tensor_3d[:, :, :-1]
    ).abs().mean().item() > 0, "Interpolation did not produce smooth transitions"
