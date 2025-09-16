import math

import polars as pl
from internal.tft_dataset import TFTDataset


def test_tft_dataset_load_data() -> None:
    data = pl.DataFrame(
        {
            "timestamp": [
                1672531200000,  # 2023-01-01
                1672617600000,  # 2023-01-02
                1672704000000,  # 2023-01-03
                1672531200000,  # 2023-01-01
                1672617600000,  # 2023-01-02
                1672704000000,  # 2023-01-03
            ],
            "open_price": [100.0, 101.0, 102.0, 50.0, 51.0, 52.0],
            "high_price": [110.0, 111.0, 112.0, 60.0, 61.0, 62.0],
            "low_price": [90.0, 91.0, 92.0, 40.0, 41.0, 42.0],
            "close_price": [105.0, 106.0, 107.0, 55.0, 56.0, 57.0],
            "volume": [1000, 1100, 1200, 500, 600, 700],
            "volume_weighted_average_price": [105.0, 106.0, 107.0, 55.0, 56.0, 57.0],
            "ticker": ["AAPL", "AAPL", "AAPL", "GOOGL", "GOOGL", "GOOGL"],
            "sector": [
                "Technology",
                "Technology",
                "Technology",
                "Technology",
                "Technology",
                "Technology",
            ],
            "industry": [
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
                "Internet Services",
                "Internet Services",
                "Internet Services",
            ],
        }
    )

    dataset = TFTDataset()

    dataset.preprocess_and_set_data(data)

    assert hasattr(dataset, "data")
    assert hasattr(dataset, "mappings")


def test_tft_dataset_get_dimensions() -> None:
    data = pl.DataFrame(
        {
            "timestamp": [
                1672531200000,  # 2023-01-01
                1672617600000,  # 2023-01-02
            ],
            "open_price": [100.0, 101.0],
            "high_price": [110.0, 111.0],
            "low_price": [90.0, 91.0],
            "close_price": [105.0, 106.0],
            "volume": [1000, 1100],
            "volume_weighted_average_price": [105.0, 106.0],
            "ticker": ["AAPL", "AAPL"],
            "sector": ["Technology", "Technology"],
            "industry": ["Consumer Electronics", "Consumer Electronics"],
        }
    )

    dataset = TFTDataset()

    dataset.preprocess_and_set_data(data)

    dimensions = dataset.get_dimensions()

    assert "encoder_categorical_features" in dimensions
    assert "encoder_continuous_features" in dimensions
    assert "decoder_categorical_features" in dimensions
    assert "decoder_continuous_features" in dimensions
    assert "static_categorical_features" in dimensions
    assert "static_continuous_features" in dimensions


def test_tft_dataset_batches() -> None:
    data = pl.DataFrame(
        {
            "timestamp": [
                1672531200000,  # 2023-01-01
                1672617600000,  # 2023-01-02
                1672704000000,  # 2023-01-03
                1672790400000,  # 2023-01-04
            ],
            "open_price": [100.0, 101.0, 102.0, 103.0],
            "high_price": [110.0, 111.0, 112.0, 113.0],
            "low_price": [90.0, 91.0, 92.0, 93.0],
            "close_price": [105.0, 106.0, 107.0, 108.0],
            "volume": [1000, 1100, 1200, 1300],
            "volume_weighted_average_price": [105.0, 106.0, 107.0, 108.0],
            "ticker": ["AAPL", "AAPL", "AAPL", "AAPL"],
            "sector": ["Technology", "Technology", "Technology", "Technology"],
            "industry": [
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
            ],
        }
    )

    dataset = TFTDataset()

    dataset.preprocess_and_set_data(data)

    expected_input_length = 2
    expected_output_length = 1

    batches = dataset.get_batches(
        data_type="predict",
        input_length=expected_input_length,
        output_length=expected_output_length,
    )

    assert isinstance(batches, list)
    assert len(batches) == 1

    for batch in batches:
        assert "encoder_categorical_features" in batch
        assert "encoder_continuous_features" in batch
        assert "decoder_categorical_features" in batch
        assert "static_categorical_features" in batch

        encoder_categorical_features = batch["encoder_categorical_features"]
        encoder_continuous_features = batch["encoder_continuous_features"]
        decoder_categorical_features = batch["decoder_categorical_features"]
        static_categorical_features = batch["static_categorical_features"]

        assert encoder_categorical_features.shape[0] == expected_input_length
        assert encoder_continuous_features.shape[0] == expected_input_length
        assert decoder_categorical_features.shape[0] == expected_output_length
        assert static_categorical_features.shape[0] == 1


def test_tft_dataset_daily_return_calculation() -> None:
    data = pl.DataFrame(
        {
            "timestamp": [
                1672531200000,  # 2023-01-01
                1672617600000,  # 2023-01-02
                1672704000000,  # 2023-01-03
                1672790400000,  # 2023-01-04
            ],
            "open_price": [100.0, 101.0, 102.0, 103.0],
            "high_price": [110.0, 111.0, 112.0, 113.0],
            "low_price": [90.0, 91.0, 92.0, 93.0],
            "close_price": [100.0, 105.0, 102.0, 108.0],  # returns: +5%, -2.86%, +5.88%
            "volume": [1000, 1100, 1200, 1300],
            "volume_weighted_average_price": [100.0, 105.0, 102.0, 108.0],
            "ticker": ["AAPL", "AAPL", "AAPL", "AAPL"],
            "sector": ["Technology", "Technology", "Technology", "Technology"],
            "industry": [
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
            ],
        }
    )

    dataset = TFTDataset()

    dataset.preprocess_and_set_data(data)

    assert "daily_return" in dataset.data.columns

    expected_rows_after_filter = 3
    assert len(dataset.data) == expected_rows_after_filter

    assert "daily_return" in dataset.continuous_columns

    daily_returns = dataset.data["daily_return"].to_list()

    expected_daily_return_count = 3
    assert len(daily_returns) == expected_daily_return_count
    assert all(isinstance(val, float) and math.isfinite(val) for val in daily_returns)


def test_tft_dataset_daily_return_targets() -> None:
    data = pl.DataFrame(
        {
            "timestamp": [
                1672531200000,  # 2023-01-01
                1672617600000,  # 2023-01-02
                1672704000000,  # 2023-01-03
                1672790400000,  # 2023-01-04
                1672876800000,  # 2023-01-05
                1672963200000,  # 2023-01-06
            ],
            "open_price": [100.0, 101.0, 102.0, 103.0, 104.0, 105.0],
            "high_price": [110.0, 111.0, 112.0, 113.0, 114.0, 115.0],
            "low_price": [90.0, 91.0, 92.0, 93.0, 94.0, 95.0],
            "close_price": [100.0, 105.0, 102.0, 108.0, 106.0, 110.0],
            "volume": [1000, 1100, 1200, 1300, 1400, 1500],
            "volume_weighted_average_price": [100.0, 105.0, 102.0, 108.0, 106.0, 110.0],
            "ticker": ["AAPL", "AAPL", "AAPL", "AAPL", "AAPL", "AAPL"],
            "sector": [
                "Technology",
                "Technology",
                "Technology",
                "Technology",
                "Technology",
                "Technology",
            ],
            "industry": [
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
            ],
        }
    )

    dataset = TFTDataset()

    dataset.preprocess_and_set_data(data)

    batches = dataset.get_batches(
        data_type="train",
        input_length=2,
        output_length=1,
    )

    assert len(batches) > 0

    for batch in batches:
        if "targets" in batch:
            targets = batch["targets"]
            assert targets.shape[1] == 1
            expected_tensor_dimensions = 2
            assert len(targets.shape) == expected_tensor_dimensions
            assert targets.shape[0] > 0
            assert targets.shape[1] == 1
