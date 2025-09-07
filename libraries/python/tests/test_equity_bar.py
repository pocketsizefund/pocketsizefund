import polars as pl
import pytest
from internal.equity_bar import equity_bar_schema
from pandera.errors import SchemaError


def test_equity_bar_schema_valid_data() -> None:
    valid_data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [1674000000],
            "open_price": [150.0],
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    validated_df = equity_bar_schema.validate(valid_data)
    assert validated_df.shape == (1, 8)


def test_equity_bar_schema_ticker_lowercase_fails() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["aapl"],
            "timestamp": [1674000000.0],
            "open_price": [150.0],
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    with pytest.raises(SchemaError):
        equity_bar_schema.validate(data)


def test_equity_bar_schema_ticker_uppercase_passes() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [1674000000.0],
            "open_price": [150.0],
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    validated_df = equity_bar_schema.validate(data)
    assert validated_df["ticker"][0] == "AAPL"


def test_equity_bar_schema_negative_timestamp() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [-1674000000.0],
            "open_price": [150.0],
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    with pytest.raises(SchemaError):
        equity_bar_schema.validate(data)


def test_equity_bar_schema_zero_timestamp() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [0.0],
            "open_price": [150.0],
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    with pytest.raises(SchemaError):
        equity_bar_schema.validate(data)


def test_equity_bar_schema_negative_prices() -> None:
    price_fields = [
        "open_price",
        "high_price",
        "low_price",
        "close_price",
        "volume_weighted_average_price",
    ]

    for field in price_fields:
        data = pl.DataFrame(
            {
                "ticker": ["AAPL"],
                "timestamp": [1674000000.0],
                "open_price": [150.0],
                "high_price": [155.0],
                "low_price": [149.0],
                "close_price": [153.0],
                "volume": [1000000],
                "volume_weighted_average_price": [152.5],
            }
        )

        data = data.with_columns(pl.lit(-1.0).alias(field))

        with pytest.raises(SchemaError):
            equity_bar_schema.validate(data)


def test_equity_bar_schema_zero_prices_not_allowed() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [1674000000.0],
            "open_price": [0.0],
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    with pytest.raises(SchemaError):
        equity_bar_schema.validate(data)


def test_equity_bar_schema_negative_volume() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [1674000000.0],
            "open_price": [150.0],
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [-1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    with pytest.raises(SchemaError):
        equity_bar_schema.validate(data)


def test_equity_bar_schema_type_coercion() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": ["1674000000.0"],  # string that can be coerced to float
            "open_price": ["150.0"],  # string that can be coerced to float
            "high_price": [155],  # int that can be coerced to float
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": ["1000000"],  # string that can be coerced to int
            "volume_weighted_average_price": [152.5],
        }
    )

    validated_df = equity_bar_schema.validate(data)
    assert validated_df["timestamp"].dtype == pl.Float64
    assert validated_df["open_price"].dtype == pl.Float64
    assert validated_df["high_price"].dtype == pl.Float64
    assert validated_df["volume"].dtype == pl.Int64


def test_equity_bar_schema_missing_required_column() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [1674000000.0],
            # Missing open_price
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    with pytest.raises((SchemaError, pl.exceptions.ColumnNotFoundError)):
        equity_bar_schema.validate(data)


def test_equity_bar_schema_null_values() -> None:
    data = pl.DataFrame(
        {
            "ticker": [None],
            "timestamp": [1674000000.0],
            "open_price": [150.0],
            "high_price": [155.0],
            "low_price": [149.0],
            "close_price": [153.0],
            "volume": [1000000],
            "volume_weighted_average_price": [152.5],
        }
    )

    with pytest.raises(SchemaError):
        equity_bar_schema.validate(data)


def test_equity_bar_schema_multiple_rows() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL", "GOOGL", "NVDA"],
            "timestamp": [1674000000.0, 1674000060.0, 1674000120.0],
            "open_price": [150.0, 100.0, 300.0],
            "high_price": [155.0, 105.0, 305.0],
            "low_price": [149.0, 99.0, 299.0],
            "close_price": [153.0, 103.0, 303.0],
            "volume": [1000000, 500000, 750000],
            "volume_weighted_average_price": [152.5, 102.0, 302.0],
        }
    )

    validated_df = equity_bar_schema.validate(data)
    assert validated_df.shape == (3, 8)
    assert validated_df["ticker"].to_list() == ["AAPL", "GOOGL", "NVDA"]
