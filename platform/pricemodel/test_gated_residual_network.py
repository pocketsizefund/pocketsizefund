from gated_residual_network import GatedResidualNetwork, GateAddNorm, GatedLinearUnit, AddNorm
from tinygrad import Tensor

input_size = 4
skip_size = 6

add_norm = AddNorm(
    input_size=input_size,
    skip_size=skip_size,
)

x_equal = Tensor.randn(2, 3, 4)  # (batch_size=2, seq_length=3, input_size=4)
skip_equal = Tensor.randn(2, 3, 4)  # (batch_size=2, seq_length=3, skip_size=4)

output = add_norm.forward(x_equal, skip_equal)

print(output)

input_size = 4
hidden_size = 8
dropout_rate = 0.1

gated_linear_unit = GatedLinearUnit(
    input_size=input_size,
    hidden_size=hidden_size,
    dropout_rate=dropout_rate,
)

x = Tensor.rand(2, 3, 4)  # (batch_size=2, seq_length=3, input_size=4)

output = gated_linear_unit.forward(x)

print(output)

gate_add_norm_custom = GateAddNorm(
    input_size=4,
    hidden_size=8,
    skip_size=6,
    dropout_rate=0.5,
)

x = Tensor.randn(2, 3, 4)  # (batch_size=2, seq_length=3, input_size=4)

skip = Tensor.randn(2, 3, 4)  # Match skip_size to hidden_size or input_size

output = gate_add_norm_custom.forward(x, skip)

print(output)

grn_with_context = GatedResidualNetwork(
    input_size=4,
    hidden_size=8,
    output_size=4,
    context_size=6,
    dropout_rate=0.2,
)

# Input tensor
x = Tensor.randn(2, 3, 4)  # (batch_size=2, seq_length=3, input_size=4)

# Context tensor (only needed for context-aware GRN)
context = Tensor.randn(2, 3, 6)  # (batch_size=2, seq_length=3, context_size=6)

output = grn_with_context.forward(x, context)

print(output)

# ==============================================================================

# outline:
# [x] gated residual network
# - [x] init
# - [x] forward
# [x] gated add norm
# - [x] init
# - [x] forward
# [x] gated linear unit
# - [x] init
# - [x] forward
# [x] add norm
# - [x] init
# - [x] forward
