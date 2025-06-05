import numpy as np
from tinygrad.tensor import Tensor

from application.predictionengine.src.predictionengine.long_short_term_memory import (
    LongShortTermMemory,
)


def test_lstm_initialization() -> None:
    lstm = LongShortTermMemory(
        input_size=32, hidden_size=64, layer_count=2, dropout_rate=0.1
    )

    assert lstm.hidden_size == 64  # noqa: PLR2004
    assert lstm.layer_count == 2  # noqa: PLR2004
    assert lstm.dropout_rate == 0.1  # noqa: PLR2004


def test_lstm_forward() -> None:
    lstm = LongShortTermMemory(
        input_size=16, hidden_size=32, layer_count=1, dropout_rate=0.0
    )

    default_range = np.random.default_rng()
    input_tensor = Tensor(default_range.standard_normal((4, 10, 16)))
    output, hidden_state = lstm.forward(input_tensor)

    assert output.shape == (4, 10, 32)
    assert isinstance(hidden_state, tuple)
    assert len(hidden_state) == 2  # noqa: PLR2004


def test_lstm_different_sequence_lengths() -> None:
    lstm = LongShortTermMemory(
        input_size=8, hidden_size=16, layer_count=1, dropout_rate=0.0
    )

    for sequence_length in [5, 10, 20]:
        default_range = np.random.default_rng()
        input_tensor = Tensor(default_range.standard_normal((2, sequence_length, 8)))
        output, _ = lstm.forward(input_tensor)

        assert output.shape == (2, sequence_length, 16)


def test_lstm_multiple_layers() -> None:
    lstm = LongShortTermMemory(
        input_size=10, hidden_size=20, layer_count=3, dropout_rate=0.0
    )

    default_range = np.random.default_rng()
    input_tensor = Tensor(default_range.standard_normal((2, 5, 10)))
    output, hidden_state = lstm.forward(input_tensor)

    assert output.shape == (2, 5, 20)
    assert isinstance(hidden_state, tuple)


def test_lstm_single_timestep() -> None:
    lstm = LongShortTermMemory(
        input_size=12, hidden_size=24, layer_count=1, dropout_rate=0.0
    )

    default_range = np.random.default_rng()
    input_tensor = Tensor(default_range.standard_normal((3, 1, 12)))
    output, _ = lstm.forward(input_tensor)

    assert output.shape == (3, 1, 24)


def test_lstm_consistency() -> None:
    lstm = LongShortTermMemory(
        input_size=6, hidden_size=12, layer_count=1, dropout_rate=0.0
    )

    default_range = np.random.default_rng()
    input_tensor = Tensor(default_range.standard_normal((1, 3, 6)))

    first_output, _ = lstm.forward(input_tensor)
    second_output, _ = lstm.forward(input_tensor)

    assert first_output.shape == second_output.shape
    assert np.allclose(first_output.numpy(), second_output.numpy(), rtol=1e-5)
