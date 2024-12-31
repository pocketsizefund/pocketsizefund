from pricemodel.lstm import Layer, LSTM
from tinygrad import Tensor


def test_layer():
    input_size = 8
    hidden_size = 16
    batch_size = 4

    layer = Layer(input_size=input_size, hidden_size=hidden_size)

    x = Tensor([[0.1] * input_size] * batch_size)

    hidden_previous_state = Tensor([[0.2] * hidden_size] * batch_size)
    cell_previous_state = Tensor([[0.3] * hidden_size] * batch_size)
    hidden_state = (hidden_previous_state, cell_previous_state)

    hidden_current_state, cell_current_state = layer.forward(x, hidden_state)

    assert hidden_current_state.shape == (batch_size, hidden_size), "Hidden state shape mismatch"
    assert cell_current_state.shape == (batch_size, hidden_size), "Cell state shape mismatch"


def test_lstm():
    input_size = 8
    hidden_size = 16
    layer_count = 2
    dropout_rate = 0.1
    batch_size = 4
    sequence_length = 10

    lstm = LSTM(
        input_size=input_size,
        hidden_size=hidden_size,
        layer_count=layer_count,
        dropout_rate=dropout_rate,
    )

    x = Tensor.zeros(batch_size, sequence_length, input_size)

    hidden_state = None

    output, (hidden_state_output, cell_state_output) = lstm.forward(x, hidden_state)

    assert output.shape == (batch_size, sequence_length, hidden_size), "Output shape mismatch"
    assert hidden_state_output.shape == (
        layer_count,
        batch_size,
        hidden_size,
    ), "Hidden state shape mismatch"
    assert cell_state_output.shape == (
        layer_count,
        batch_size,
        hidden_size,
    ), "Cell state shape mismatch"
