from interpolation import TimeDistributedInterpolation
from tinygrad import Tensor


output_size = 10

time_distributed_interpolation = TimeDistributedInterpolation(output_size)

x_1d = Tensor([1.0, 2.0, 3.0])

output_1d = time_distributed_interpolation.interpolate(x_1d)

print("Interpolate (1D) output:", output_1d)
print("Interpolate (1D) output shape:", output_1d.shape)

# Call the forward method with 1D input
forward_output_1d = time_distributed_interpolation.forward(x_1d)

# Check the shape and content
print("Forward (1D) output:", forward_output_1d)
print("Forward (1D) output shape:", forward_output_1d.shape)

# Input tensor (3D case)
batch_size = 2
sequence_length = 5
feature_size = 4
x_3d = Tensor.rand(batch_size, sequence_length, feature_size)

# Call the forward method
output_3d = time_distributed_interpolation.forward(x_3d)

# Check the shape and content
print("Forward (3D) output shape:", output_3d.shape)

# Edge case: Empty tensor
x_empty = Tensor([])
try:
    output_empty = time_distributed_interpolation.forward(x_empty)
    print("Forward (empty) output shape:", output_empty.shape)
except Exception as e:
    print("Error with empty tensor:", e)


# ==============================================================================

# outline:
# [x] time distributed interpolation
# - [x] init
# - [x] interpolate
# - [x] forward
