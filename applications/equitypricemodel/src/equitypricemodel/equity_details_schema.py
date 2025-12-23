import pandera.polars as pa

equity_details_schema = pa.DataFrameSchema(
    {
        "sector": pa.Column(
            dtype=str,
            default="NOT AVAILABLE",
        ),
        "industry": pa.Column(
            dtype=str,
            default="NOT AVAILABLE",
        ),
    },
    coerce=True,
    checks=[
        pa.Check(
            lambda s: s.upper() == s,
            error="Sector and industry must be uppercase",
            element_wise=True,
        ),
        pa.Check(
            lambda s: s.strip() == s,
            error="Sector and industry must be stripped",
            element_wise=True,
        ),
    ],
)
