import pandera.polars as pa
import polars as pl
from pandera.polars import PolarsData


def is_uppercase(data: PolarsData) -> pl.LazyFrame:
    return data.lazyframe.select(
        pl.col(data.key).str.to_uppercase() == pl.col(data.key)
    )


def is_stripped(data: PolarsData) -> pl.LazyFrame:
    return data.lazyframe.select(pl.col(data.key).str.strip_chars() == pl.col(data.key))


company_information_schema = pa.DataFrameSchema(
    {
        "sector": pa.Column(
            dtype=str,
            nullable=False,
            coerce=True,
            checks=[
                pa.Check(is_uppercase),
                pa.Check(is_stripped),
            ],
        ),
        "industry": pa.Column(
            dtype=str,
            nullable=False,
            coerce=True,
            checks=[
                pa.Check(is_uppercase),
                pa.Check(is_stripped),
            ],
        ),
    }
)
