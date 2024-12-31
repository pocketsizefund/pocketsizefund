from pricemodel.variable_selection_network import VariableSelectionNetwork
from tinygrad import Tensor
from typing import Dict


def test_variable_selection_network():
    static_categoricals = ["ticker"]
    hidden_continuous_size: int = 8
    input_sizes: Dict[str, int] = {name: hidden_continuous_size for name in static_categoricals}
    hidden_size: int = 16
    input_embedding_flags: Dict[str, bool] = {
        "ticker": True,
    }
    dropout_rate = 0.1
    prescalers = {}

    vsn = VariableSelectionNetwork(
        input_sizes=input_sizes,
        hidden_size=hidden_size,
        input_embedding_flags=input_embedding_flags,
        dropout_rate=dropout_rate,
        prescalers=prescalers,
    )

    feature_count = 1
    embedding_dimension = hidden_continuous_size

    tensor = Tensor.uniform(
        (feature_count, embedding_dimension),
        low=-0.1,
        high=0.1,
    )

    x = {"ticker": tensor}

    outputs, sparse_weights = vsn.forward(x)

    assert outputs.shape == (feature_count, hidden_size), "Output shape mismatch"
    assert sparse_weights.shape == (1, 1, 1), "Sparse weights shape mismatch"
