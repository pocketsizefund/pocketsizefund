from pricemodel.long_short_term_memory import LongShortTermMemory
from tinygrad import Tensor


def test_long_short_term_memory():
    input_size = 20
    hidden_size = 64 + 15
    layer_count = 3
    dropout_rate = 0.10
    # input_size = 9  # NOTE: change to match trainer script
    # hidden_size = 16  # NOTE: change to match trainer script
    # layer_count = 3  # NOTE: change to match trainer script
    # dropout_rate = 0.1  # NOTE: change to match trainer script

    long_short_term_memory = LongShortTermMemory(
        input_size=input_size,
        hidden_size=hidden_size,
        layer_count=layer_count,
        dropout_rate=dropout_rate,
    )

    assert long_short_term_memory.hidden_size == 16, "Hidden size mismatch"
    assert long_short_term_memory.layer_count == 3, "Layer count mismatch"
    assert long_short_term_memory.dropout_rate == 0.1, "Dropout rate mismatch"
    assert len(long_short_term_memory.layers) == 3, "Layers count mismatch"

    batch_size = 5
    sequence_length = 30
    feature_count = 9

    input = Tensor.zeros(batch_size, sequence_length, feature_count)

    output, (hidden_state_output, cell_state_output) = long_short_term_memory.forward(input)

    assert output.shape == (5, 16), "Output shape mismatch"
    assert hidden_state_output.shape == (3, 5, 16), "Hidden state shape mismatch"
    assert cell_state_output.shape == (3, 5, 16), "Cell state shape mismatch"
