from datetime import date

import pytest
from internal.equity_bar import EquityBar
from pydantic import ValidationError


def test_equity_bar_valid_creation() -> None:
    open_price = 150.0
    high_price = 155.0
    low_price = 149.0
    close_price = 153.0
    volume = 1000000
    volume_weighted_average_price = 152.5

    equity_bar = EquityBar(
        ticker="AAPL",
        timestamp=date(2023, 1, 15),
        open_price=open_price,
        high_price=high_price,
        low_price=low_price,
        close_price=close_price,
        volume=volume,
        volume_weighted_average_price=volume_weighted_average_price,
    )

    assert equity_bar.ticker == "AAPL"
    assert equity_bar.timestamp == date(2023, 1, 15)
    assert equity_bar.open_price == open_price
    assert equity_bar.high_price == high_price
    assert equity_bar.low_price == low_price
    assert equity_bar.close_price == close_price
    assert equity_bar.volume == volume
    assert equity_bar.volume_weighted_average_price == volume_weighted_average_price


def test_equity_bar_ticker_validation_uppercase() -> None:
    equity_bar = EquityBar(
        ticker="aapl",
        timestamp=date(2023, 1, 15),
        open_price=150.0,
        high_price=155.0,
        low_price=149.0,
        close_price=153.0,
        volume=1000000,
        volume_weighted_average_price=152.5,
    )

    assert equity_bar.ticker == "AAPL"


def test_equity_bar_ticker_validation_strip_whitespace() -> None:
    equity_bar = EquityBar(
        ticker="  GOOGL  ",
        timestamp=date(2023, 1, 15),
        open_price=100.0,
        high_price=105.0,
        low_price=99.0,
        close_price=103.0,
        volume=500000,
        volume_weighted_average_price=102.0,
    )

    assert equity_bar.ticker == "GOOGL"


def test_equity_bar_ticker_validation_empty() -> None:
    with pytest.raises(ValidationError) as exc_info:
        EquityBar(
            ticker="",
            timestamp=date(2023, 1, 15),
            open_price=150.0,
            high_price=155.0,
            low_price=149.0,
            close_price=153.0,
            volume=1000000,
            volume_weighted_average_price=152.5,
        )

    assert "Ticker cannot be empty" in str(exc_info.value)


def test_equity_bar_ticker_validation_whitespace_only() -> None:
    with pytest.raises(ValidationError) as exc_info:
        EquityBar(
            ticker="   ",
            timestamp=date(2023, 1, 15),
            open_price=150.0,
            high_price=155.0,
            low_price=149.0,
            close_price=153.0,
            volume=1000000,
            volume_weighted_average_price=152.5,
        )

    assert "Ticker cannot be empty" in str(exc_info.value)


def test_equity_bar_negative_price_validation() -> None:
    with pytest.raises(ValidationError) as exc_info:
        EquityBar(
            ticker="AAPL",
            timestamp=date(2023, 1, 15),
            open_price=-150.0,
            high_price=155.0,
            low_price=149.0,
            close_price=153.0,
            volume=1000000,
            volume_weighted_average_price=152.5,
        )

    assert "Price cannot be negative" in str(exc_info.value)


def test_equity_bar_zero_price_allowed() -> None:
    equity_bar = EquityBar(
        ticker="AAPL",
        timestamp=date(2023, 1, 15),
        open_price=0.0,
        high_price=0.0,
        low_price=0.0,
        close_price=0.0,
        volume=1000000,
        volume_weighted_average_price=0.0,
    )

    assert equity_bar.open_price == 0.0
    assert equity_bar.high_price == 0.0
    assert equity_bar.low_price == 0.0
    assert equity_bar.close_price == 0.0


def test_equity_bar_timestamp_string_iso_format() -> None:
    equity_bar = EquityBar(
        ticker="AAPL",
        timestamp=date.fromisoformat("2023-06-15"),
        open_price=150.0,
        high_price=155.0,
        low_price=149.0,
        close_price=153.0,
        volume=1000000,
        volume_weighted_average_price=152.5,
    )

    assert equity_bar.timestamp == date(2023, 6, 15)


def test_equity_bar_all_price_fields_negative() -> None:
    price_fields = ["open_price", "high_price", "low_price", "close_price"]

    for field in price_fields:
        kwargs = {
            "ticker": "AAPL",
            "timestamp": date(2023, 1, 15),
            "open_price": 150.0,
            "high_price": 155.0,
            "low_price": 149.0,
            "close_price": 153.0,
            "volume": 1000000,
            "volume_weighted_average_price": 152.5,
        }
        kwargs[field] = -1.0

        with pytest.raises(ValidationError) as exc_info:
            EquityBar(**kwargs)

        assert "Price cannot be negative" in str(exc_info.value)


def test_equity_bar_large_volume() -> None:
    volume = 10**12  # Large volume for testing

    equity_bar = EquityBar(
        ticker="NVDA",
        timestamp=date(2023, 1, 15),
        open_price=300.0,
        high_price=305.0,
        low_price=299.0,
        close_price=303.0,
        volume=volume,
        volume_weighted_average_price=302.0,
    )

    assert equity_bar.volume == volume


def test_equity_bar_special_ticker_symbols() -> None:
    special_tickers = ["brk.b", "BF-B", "META"]

    for ticker in special_tickers:
        equity_bar = EquityBar(
            ticker=ticker,
            timestamp=date(2023, 1, 15),
            open_price=100.0,
            high_price=105.0,
            low_price=99.0,
            close_price=103.0,
            volume=1000000,
            volume_weighted_average_price=102.0,
        )

        assert equity_bar.ticker == ticker.upper()
