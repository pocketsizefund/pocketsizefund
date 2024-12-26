from variable_selection_network import VariableSelectionNetwork
from tinygrad.tensor import Tensor
import numpy as np

# Example Preprocessed Data
preprocessed_data = {
    "open": Tensor(
        np.array(
            [
                [145.0, 146.0],  # AAPL
                [310.0, 311.5],  # MSFT
            ]
        )
    ),  # Shape: (2 tickers, 2 timestamps)
    "high": Tensor(
        np.array(
            [
                [147.0, 148.0],  # AAPL
                [312.0, 313.0],  # MSFT
            ]
        )
    ),
    "low": Tensor(
        np.array(
            [
                [144.5, 145.5],  # AAPL
                [309.0, 310.5],  # MSFT
            ]
        )
    ),
    "close": Tensor(
        np.array(
            [
                [146.0, 147.0],  # AAPL
                [311.5, 312.5],  # MSFT
            ]
        )
    ),
    "volume": Tensor(
        np.array(
            [
                [1_000_000, 1_200_000],  # AAPL
                [500_000, 600_000],  # MSFT
            ]
        )
    ),
    # Assume "ticker" is one-hot encoded
    "ticker": Tensor(
        np.array(
            [
                [1, 0],  # AAPL
                [0, 1],  # MSFT
            ]
        )
    ),  # Shape: (2 tickers, 2 one-hot vectors)
}

context = Tensor(
    np.array(
        [
            [0.5, 0.8, 0.3],  # Example context features for AAPL
            [0.7, 0.6, 0.9],  # Example context features for MSFT
        ]
    )
)  # Shape: (2 tickers, context_size)

# Initialize the network
vsn = VariableSelectionNetwork(
    input_sizes={"open": 2, "high": 2, "low": 2, "close": 2, "volume": 2, "ticker": 2},
    hidden_size=8,
    context_size=3,
)

# Forward pass
outputs, sparse_weights = vsn.forward(preprocessed_data, context)

# Outputs
print("Outputs:", outputs.shape)  # e.g., (2, 2, 8)
print("Sparse Weights:", sparse_weights.shape)  # e.g., (2, 2, 1, 6)


# ==============================================================================

# outline:
# [ ] variable selection network
# - [ ] init
# - [ ] forward
