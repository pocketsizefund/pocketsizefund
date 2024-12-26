from lstm import LSTM, Layer
from tinygrad import Tensor


layer = Layer(
    input_size=8,  # Example input size
    hidden_size=16,  # Example hidden size
)

# Input tensor
x = Tensor.randn(2, 3, 8)  # (batch_size=2, seq_length=3, input_size=8)

# Hidden and cell states
hidden_previous_state = Tensor.randn(2, 3, 16)  # (batch_size=2, seq_length=3, hidden_size=16)
cell_previous_state = Tensor.randn(2, 3, 16)  # (batch_size=2, seq_length=3, hidden_size=16)

hidden_state = (hidden_previous_state, cell_previous_state)

output = layer.forward(x, hidden_state)

print(output[0].shape)
print(output[1].shape)

lstm = LSTM(
    input_size=8,  # Input tensor dimension
    hidden_size=16,  # Hidden state dimension
    layer_count=2,  # Number of LSTM layers
    dropout_rate=0.1,  # Dropout rate
    # batch_first=True,  # Batch-first input format
)

x = Tensor.randn(2, 5, 8)  # (batch_size=2, seq_length=5, input_size=8)

# Hidden state initialization (optional)
hidden_state = (
    Tensor.zeros(2, 2, 16),  # (layer_count=2, batch_size=2, hidden_size=16)
    Tensor.zeros(2, 2, 16),  # (layer_count=2, batch_size=2, hidden_size=16)
)

output, (hidden_state, cell_state) = lstm.forward(x, hidden_state)

print(output.shape)
print(hidden_state.shape)
print(cell_state.shape)

# ==============================================================================

# outline:
# [x] lstm
# - [x] init
# - [x] forward
# [x] layer
# - [x] init
# - [x] forward
