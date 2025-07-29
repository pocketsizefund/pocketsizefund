import polars as pl
from internal.dataset import TemporalFusionTransformerDataset


def test_dataset_load_data() -> None:
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
            "volume": [1000.0, 1100.0, 1200.0, 500.0, 600.0, 700.0],
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
            "is_holiday": [True, False, False, True, False, False],
        }
    )

    dataset = TemporalFusionTransformerDataset(data=data)

    assert hasattr(dataset, "data")
    assert hasattr(dataset, "mappings")


def test_dataset_get_dimensions() -> None:
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
            "volume": [1000.0, 1100.0],
            "volume_weighted_average_price": [105.0, 106.0],
            "ticker": ["AAPL", "AAPL"],
            "sector": ["Technology", "Technology"],
            "industry": ["Consumer Electronics", "Consumer Electronics"],
            "is_holiday": [True, False],
        }
    )

    dataset = TemporalFusionTransformerDataset(data=data)

    dimensions = dataset.get_dimensions()

    assert "encoder_categorical_features" in dimensions
    assert "encoder_continuous_features" in dimensions
    assert "decoder_categorical_features" in dimensions
    assert "decoder_continuous_features" in dimensions
    assert "static_categorical_features" in dimensions
    assert "static_continuous_features" in dimensions


def test_dataset_batches() -> None:
    data = pl.DataFrame(
        {
            "timestamp": [
                1672531200000,  # 2023-01-01
                1672617600000,  # 2023-01-02
                1672704000000,  # 2023-01-03
            ],
            "open_price": [100.0, 101.0, 102.0],
            "high_price": [110.0, 111.0, 112.0],
            "low_price": [90.0, 91.0, 92.0],
            "close_price": [105.0, 106.0, 107.0],
            "volume": [1000.0, 1100.0, 1200.0],
            "volume_weighted_average_price": [105.0, 106.0, 107.0],
            "ticker": ["AAPL", "AAPL", "AAPL"],
            "sector": ["Technology", "Technology", "Technology"],
            "industry": [
                "Consumer Electronics",
                "Consumer Electronics",
                "Consumer Electronics",
            ],
            "is_holiday": [True, False, False],
        }
    )

    dataset = TemporalFusionTransformerDataset(data=data)

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
