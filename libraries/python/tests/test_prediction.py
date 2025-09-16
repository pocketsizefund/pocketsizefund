from datetime import UTC, datetime, timedelta

import polars as pl
import pytest
from internal.prediction import prediction_schema
from pandera.errors import SchemaError


def test_prediction_schema_valid_data() -> None:
    base_date = datetime(2024, 1, 1, tzinfo=UTC)

    valid_data = pl.DataFrame(
        {
            "ticker": ["AAPL"] * 7,
            "timestamp": [
                (base_date + timedelta(days=i)).timestamp() for i in range(7)
            ],
            "quantile_10": [100.0, 101.0, 102.0, 103.0, 104.0, 105.0, 106.0],
            "quantile_50": [150.0, 151.0, 152.0, 153.0, 154.0, 155.0, 156.0],
            "quantile_90": [200.0, 201.0, 202.0, 203.0, 204.0, 205.0, 206.0],
        }
    )

    validated_df = prediction_schema.validate(valid_data)
    assert validated_df.shape == (7, 5)


def test_prediction_schema_ticker_lowercase_fails() -> None:
    base_date = datetime(2024, 1, 1, tzinfo=UTC)

    data = pl.DataFrame(
        {
            "ticker": ["aapl"] * 7,  # lowercase should fail
            "timestamp": [
                (base_date + timedelta(days=i)).timestamp() for i in range(7)
            ],
            "quantile_10": [100.0] * 7,
            "quantile_50": [150.0] * 7,
            "quantile_90": [200.0] * 7,
        }
    )

    with pytest.raises(SchemaError):
        prediction_schema.validate(data)


def test_prediction_schema_negative_timestamp_fails() -> None:
    base_date = datetime(2024, 1, 1, tzinfo=UTC)

    data = pl.DataFrame(
        {
            "ticker": ["AAPL"] * 7,
            "timestamp": [
                (base_date + timedelta(days=i)).timestamp() for i in range(7)
            ],
            "quantile_10": [100.0] * 7,
            "quantile_50": [150.0] * 7,
            "quantile_90": [200.0] * 7,
        }
    )

    data[1, "timestamp"] = -1.0  # introduce a negative timestamp

    with pytest.raises(SchemaError):
        prediction_schema.validate(data)


def test_prediction_schema_duplicate_ticker_timestamp_fails() -> None:
    base_date = datetime(2024, 1, 1, tzinfo=UTC)

    data = pl.DataFrame(
        {
            "ticker": ["AAPL"] * 8,  # 8 rows but duplicate timestamp
            "timestamp": [(base_date + timedelta(days=i)).timestamp() for i in range(7)]
            + [(base_date + timedelta(days=0)).timestamp()],  # duplicate timestamp
            "quantile_10": [100.0] * 8,
            "quantile_50": [150.0] * 8,
            "quantile_90": [200.0] * 8,
        }
    )

    with pytest.raises(SchemaError):
        prediction_schema.validate(data)


def test_prediction_schema_multiple_tickers_same_dates() -> None:
    base_date = datetime(2024, 1, 1, tzinfo=UTC)

    valid_data = pl.DataFrame(
        {
            "ticker": (["AAPL"] * 7) + (["GOOGL"] * 7),
            "timestamp": [(base_date + timedelta(days=i)).timestamp() for i in range(7)]
            * 2,  # same timestamps for both tickers
            "quantile_10": [100.0] * 14,
            "quantile_50": [150.0] * 14,
            "quantile_90": [200.0] * 14,
        }
    )

    validated_df = prediction_schema.validate(valid_data)
    assert validated_df.shape == (14, 5)


def test_prediction_schema_multiple_tickers_different_dates_fails() -> None:
    base_date = datetime(2024, 1, 1, tzinfo=UTC)

    data = pl.DataFrame(
        {
            "ticker": (["AAPL"] * 7) + (["GOOGL"] * 7),
            "timestamp": [
                (base_date + timedelta(days=i)).timestamp() for i in range(14)
            ],
            "quantile_10": [100.0] * 14,
            "quantile_50": [150.0] * 14,
            "quantile_90": [200.0] * 14,
        }
    )

    with pytest.raises(
        SchemaError, match="Expected all tickers to have the same dates"
    ):
        prediction_schema.validate(data)


def test_prediction_schema_wrong_date_count_per_ticker_fails() -> None:
    base_date = datetime(2024, 1, 1, tzinfo=UTC)

    data = pl.DataFrame(
        {
            "ticker": ["AAPL"] * 5,  # only 5 dates instead of 7
            "timestamp": [
                (base_date + timedelta(days=i)).timestamp() for i in range(5)
            ],
            "quantile_10": [100.0] * 5,
            "quantile_50": [150.0] * 5,
            "quantile_90": [200.0] * 5,
        }
    )

    with pytest.raises(
        SchemaError, match="Each ticker must have exactly 7 unique dates"
    ):
        prediction_schema.validate(data)


def test_prediction_schema_float_quantile_values() -> None:
    base_date = datetime(2024, 1, 1, tzinfo=UTC)

    valid_data = pl.DataFrame(
        {
            "ticker": ["AAPL"] * 7,
            "timestamp": [
                (base_date + timedelta(days=i)).timestamp() for i in range(7)
            ],
            "quantile_10": [100.5, 101.7, 102.3, 103.8, 104.2, 105.9, 106.1],
            "quantile_50": [150.1, 151.4, 152.6, 153.2, 154.8, 155.3, 156.7],
            "quantile_90": [200.9, 201.2, 202.5, 203.7, 204.4, 205.6, 206.8],
        }
    )

    validated_df = prediction_schema.validate(valid_data)
    assert validated_df.shape == (7, 5)
