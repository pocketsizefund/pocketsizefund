import pandera.polars as pa
import polars as pl
from pandera.polars import PolarsData


def is_uppercase(data: PolarsData) -> pl.LazyFrame:
    return data.lazyframe.select(
        pl.col(data.key).str.to_uppercase() == pl.col(data.key)
    )


def is_positive(data: PolarsData) -> pl.LazyFrame:
    return data.lazyframe.select(pl.col(data.key) > 0)


equity_bar_schema = pa.DataFrameSchema(
    {
        "ticker": pa.Column(
            dtype=str,
            nullable=False,
            coerce=True,
            checks=[pa.Check(is_uppercase)],
        ),
        "timestamp": pa.Column(
            dtype=int,
            nullable=False,
            coerce=True,
            checks=[pa.Check(is_positive)],
        ),
        "open_price": pa.Column(
            dtype=float,
            nullable=False,
            coerce=True,
            checks=[pa.Check(is_positive)],
        ),
        "high_price": pa.Column(
            dtype=float,
            nullable=False,
            coerce=True,
            checks=[pa.Check(is_positive)],
        ),
        "low_price": pa.Column(
            dtype=float,
            nullable=False,
            coerce=True,
            checks=[pa.Check(is_positive)],
        ),
        "close_price": pa.Column(
            dtype=float,
            nullable=False,
            coerce=True,
            checks=[pa.Check(is_positive)],
        ),
        "volume": pa.Column(
            dtype=int,
            nullable=False,
            coerce=True,
            checks=[pa.Check(is_positive)],
        ),
        "volume_weighted_average_price": pa.Column(
            dtype=float,
            nullable=True,
            coerce=True,
            # allow missing value or enforce > 0 when present
            checks=[
                pa.Check(
                    lambda df: df.lazyframe.select(
                        pl.col(df.key).is_null() | (pl.col(df.key) > 0)
                    )
                )
            ],
        ),
    },
    strict="filter",  # allows DuckDB partion columns
    ordered=True,
    name="equity_bar",
)
