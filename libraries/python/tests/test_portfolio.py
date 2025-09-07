from datetime import UTC, datetime

import polars as pl
import pytest
from internal.portfolio import portfolio_schema
from pandera.errors import SchemaError


def test_portfolio_schema_valid_data() -> None:
    valid_data = pl.DataFrame(
        {
            "ticker": [
                "AAPL",
                "GOOGL",
                "MSFT",
                "AMZN",
                "TSLA",
                "NVDA",
                "META",
                "NFLX",
                "BABA",
                "CRM",
                "AMD",
                "INTC",
                "ORCL",
                "ADBE",
                "PYPL",
                "SHOP",
                "SPOT",
                "ROKU",
                "ZM",
                "DOCU",
            ],
            "timestamp": [datetime(2025, 1, 1, 0, 0, 0, 0, tzinfo=UTC).timestamp()]
            * 20,
            "side": (["LONG"] * 10) + (["SHORT"] * 10),
            "dollar_amount": [1000.0] * 20,  # Equal amounts for balanced portfolio
        }
    )

    validated_df = portfolio_schema.validate(valid_data)
    assert validated_df.shape == (20, 4)


def test_portfolio_schema_ticker_lowercase_fails() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["aapl"],  # lowercase should fail
            "timestamp": [datetime(2025, 1, 1, 0, 0, 0, 0, tzinfo=UTC).timestamp()],
            "side": ["LONG"],
            "dollar_amount": [1000.0],
        }
    )

    with pytest.raises(SchemaError):
        portfolio_schema.validate(data)


def test_portfolio_schema_invalid_side_fails() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [datetime(2025, 1, 1, 0, 0, 0, 0, tzinfo=UTC).timestamp()],
            "side": ["BUY"],  # Invalid side value
            "dollar_amount": [1000.0],
        }
    )

    with pytest.raises(SchemaError):
        portfolio_schema.validate(data)


def test_portfolio_schema_negative_dollar_amount_fails() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [datetime(2025, 1, 1, 0, 0, 0, 0, tzinfo=UTC).timestamp()],
            "side": ["LONG"],
            "dollar_amount": [-1000.0],  # Negative amount should fail
        }
    )

    with pytest.raises(SchemaError):
        portfolio_schema.validate(data)


def test_portfolio_schema_unbalanced_sides_fails() -> None:
    data = pl.DataFrame(
        {
            "ticker": [
                "AAPL",
                "GOOGL",
                "MSFT",
                "AMZN",
                "TSLA",
                "NVDA",
                "META",
                "NFLX",
                "BABA",
                "CRM",
                "AMD",
                "INTC",
                "ORCL",
                "ADBE",
                "PYPL",
                "SHOP",
                "SPOT",
                "ROKU",
                "ZM",
                "DOCU",
            ],
            "timestamp": [datetime(2025, 1, 1, 0, 0, 0, 0, tzinfo=UTC).timestamp()]
            * 20,
            "side": ["LONG"] * 15 + ["SHORT"] * 5,  # Unbalanced: 15 LONG, 5 SHORT
            "dollar_amount": [1000.0] * 20,
        }
    )

    with pytest.raises(SchemaError, match="Expected 10 long side positions, found: 15"):
        portfolio_schema.validate(data)


def test_portfolio_schema_imbalanced_dollar_amounts_fails() -> None:
    data = pl.DataFrame(
        {
            "ticker": [
                "AAPL",
                "GOOGL",
                "MSFT",
                "AMZN",
                "TSLA",
                "NVDA",
                "META",
                "NFLX",
                "BABA",
                "CRM",
                "AMD",
                "INTC",
                "ORCL",
                "ADBE",
                "PYPL",
                "SHOP",
                "SPOT",
                "ROKU",
                "ZM",
                "DOCU",
            ],
            "timestamp": [datetime(2025, 1, 1, 0, 0, 0, 0, tzinfo=UTC).timestamp()]
            * 20,
            "side": (["LONG"] * 10) + (["SHORT"] * 10),
            "dollar_amount": ([2000.0] * 10)
            + ([500.0] * 10),  # Very imbalanced amounts
        }
    )

    with pytest.raises(SchemaError, match="long and short dollar amount sums"):
        portfolio_schema.validate(data)


def test_portfolio_schema_duplicate_tickers_fails() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL", "AAPL"],  # Duplicate ticker
            "timestamp": [datetime(2025, 1, 1, 0, 0, 0, 0, tzinfo=UTC).timestamp()] * 2,
            "side": ["LONG", "SHORT"],
            "dollar_amount": [1000.0, 1000.0],
        }
    )

    with pytest.raises(SchemaError):
        portfolio_schema.validate(data)


def test_portfolio_schema_zero_timestamp_fails() -> None:
    data = pl.DataFrame(
        {
            "ticker": ["AAPL"],
            "timestamp": [0.0],  # Zero timestamp should fail
            "side": ["LONG"],
            "dollar_amount": [1000.0],
        }
    )

    with pytest.raises(SchemaError):
        portfolio_schema.validate(data)
