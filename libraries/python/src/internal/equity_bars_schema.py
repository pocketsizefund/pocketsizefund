import pandera.polars as pa
import polars as pl

equity_bars_schema = pa.DataFrameSchema(
    {
        "ticker": pa.Column(
            dtype=str,
            checks=[
                pa.Check(
                    lambda s: s.upper() == s,
                    error="Ticker must be uppercase",
                    element_wise=True,
                )
            ],
        ),
        "timestamp": pa.Column(
            dtype=pl.Float64,
            checks=[pa.Check.greater_than(0)],
        ),
        "open_price": pa.Column(
            dtype=float,
            checks=[pa.Check.greater_than(0)],
        ),
        "high_price": pa.Column(
            dtype=float,
            checks=[pa.Check.greater_than(0)],
        ),
        "low_price": pa.Column(
            dtype=float,
            checks=[pa.Check.greater_than(0)],
        ),
        "close_price": pa.Column(
            dtype=float,
            checks=[pa.Check.greater_than(0)],
        ),
        "volume": pa.Column(
            dtype=int,
            checks=[pa.Check.greater_than_or_equal_to(0)],
        ),
        "volume_weighted_average_price": pa.Column(
            dtype=float,
            nullable=True,
            checks=[pa.Check.greater_than_or_equal_to(0)],
        ),
    },
    unique=["ticker", "timestamp"],
    strict="filter",  # allows DuckDB partion columns
    ordered=True,
    name="equity_bar",
    coerce=True,
)
