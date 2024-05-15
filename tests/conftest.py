import pytest
import polars as pl


@pytest.fixture
def ticker_data():
    return pl.read_csv("data/test_data.csv")
