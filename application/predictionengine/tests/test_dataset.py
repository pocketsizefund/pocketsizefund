from typing import NamedTuple

import polars as pl
import pytest

from application.predictionengine.src.predictionengine.dataset import DataSet


def test_dataset_initialization() -> None:
    dataset = DataSet(
        batch_size=2,
        sequence_length=3,
        sample_count=3,
    )

    assert dataset.batch_size == 2  # noqa: PLR2004
    assert dataset.sequence_length == 3  # noqa: PLR2004
    assert dataset.sample_count == 3  # noqa: PLR2004
    assert len(dataset) == 2  # noqa: PLR2004


def test_dataset_load_data() -> None:
    data = pl.DataFrame(
        {
            "timestamp": [
                "2023-01-01",
                "2023-01-02",
                "2023-01-03",
                "2023-01-01",
                "2023-01-02",
                "2023-01-03",
            ],
            "open_price": [100.0, 101.0, 102.0, 50.0, 51.0, 52.0],
            "high_price": [110.0, 111.0, 112.0, 60.0, 61.0, 62.0],
            "low_price": [90.0, 91.0, 92.0, 40.0, 41.0, 42.0],
            "close_price": [105.0, 106.0, 107.0, 55.0, 56.0, 57.0],
            "volume": [1000.0, 1100.0, 1200.0, 500.0, 600.0, 700.0],
            "volume_weighted_average_price": [105.0, 106.0, 107.0, 55.0, 56.0, 57.0],
            "ticker": ["AAPL", "AAPL", "AAPL", "GOOGL", "GOOGL", "GOOGL"],
        }
    )

    dataset = DataSet(
        batch_size=1,
        sequence_length=3,
        sample_count=6,
    )

    dataset.load_data(data)

    assert hasattr(dataset, "data")
    assert hasattr(dataset, "preprocessors")
    assert "indices" in dataset.preprocessors
    assert "ticker_encoder" in dataset.preprocessors


def test_dataset_get_preprocessors() -> None:
    data = pl.DataFrame(
        {
            "timestamp": ["2023-01-01", "2023-01-02"],
            "open_price": [100.0, 101.0],
            "high_price": [110.0, 111.0],
            "low_price": [90.0, 91.0],
            "close_price": [105.0, 106.0],
            "volume": [1000.0, 1100.0],
            "volume_weighted_average_price": [105.0, 106.0],
            "ticker": ["AAPL", "AAPL"],
        }
    )

    dataset = DataSet(
        batch_size=1,
        sequence_length=2,
        sample_count=2,
    )

    dataset.load_data(data)
    preprocessors = dataset.get_preprocessors()

    assert "means_by_ticker" in preprocessors
    assert "standard_deviations_by_ticker" in preprocessors
    assert "ticker_encoder" in preprocessors
    assert "indices" in preprocessors


def test_dataset_batches() -> None:
    data = pl.DataFrame(
        {
            "timestamp": ["2023-01-01", "2023-01-02", "2023-01-03"],
            "open_price": [100.0, 101.0, 102.0],
            "high_price": [110.0, 111.0, 112.0],
            "low_price": [90.0, 91.0, 92.0],
            "close_price": [105.0, 106.0, 107.0],
            "volume": [1000.0, 1100.0, 1200.0],
            "volume_weighted_average_price": [105.0, 106.0, 107.0],
            "ticker": ["AAPL", "AAPL", "AAPL"],
        }
    )

    dataset = DataSet(
        batch_size=1,
        sequence_length=2,
        sample_count=3,
    )

    dataset.load_data(data)

    class Expected(NamedTuple):
        batch_size: int = 1
        sequence_length: int = 2
        sample_count: int = 3
        observations: int = 2
        features: int = 6
        target: int = 1

    expected = Expected()

    batch_count = 0
    for tickers, features, targets in dataset.batches():
        batch_count += 1
        assert tickers.shape[0] == expected.batch_size
        assert features.shape == (
            expected.batch_size,
            expected.sequence_length,
            expected.features,
        )
        assert targets.shape == (expected.batch_size, expected.target)

    assert batch_count > 0


def test_dataset_preprocessors_validation() -> None:
    dataset = DataSet(
        batch_size=1,
        sequence_length=2,
        sample_count=2,
    )

    with pytest.raises(ValueError, match="Preprocessors have not been initialized"):
        _ = dataset.get_preprocessors()
