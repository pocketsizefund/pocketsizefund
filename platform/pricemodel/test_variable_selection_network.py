from typing import Dict
from gated_residual_network import GatedResidualNetwork
from tinygrad import Tensor
from tinygrad.nn import Linear
from variable_selection_network import VariableSelectionNetwork


static_categoricals = ["ticker"]
# time_varying_reals_encoder = [
#     "close_price",
#     "high_price",
#     "low_price",
#     "open_price",
#     "volume",
#     "volume_weighted_average_price",
# ]
hidden_continuous_size: int = 8
input_sizes: Dict[str, int] = {name: hidden_continuous_size for name in static_categoricals}
hidden_size: int = 16
input_embedding_flags: Dict[str, bool] = {
    "ticker": True,
}
dropout_rate = 0.1
prescalers = {}

variable_selection_network = VariableSelectionNetwork(
    input_sizes=input_sizes,
    hidden_size=hidden_size,
    input_embedding_flags=input_embedding_flags,
    dropout_rate=dropout_rate,
    prescalers=prescalers,
)

print(variable_selection_network)

feature_count = 1
embedding_dimension = hidden_continuous_size

tensor = Tensor.uniform(
    (feature_count, embedding_dimension),
    low=-0.1,
    high=0.1,
)

x = {"ticker": tensor}

outputs, sparse_weights = variable_selection_network.forward(x)

print(outputs)
print(sparse_weights)


# ==============================================================================

# outline:
# [x] test input variables
# [x] variable selection network
# - [x] init
# - [x] forward
