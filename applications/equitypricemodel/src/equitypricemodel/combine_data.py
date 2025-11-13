import sys

import polars as pl

from .categories_schema import categories_schema

categories_csv_path = sys.argv[1]
equity_bars_csv_path = sys.argv[2]
output_csv_path = sys.argv[3]

categories_data = pl.read_csv(categories_csv_path)

categories_data = categories_schema.validate(categories_data)

equity_bars_data = pl.read_csv(equity_bars_csv_path)

consolidated_data = categories_data.join(equity_bars_data, on="ticker", how="inner")

retained_columns = (
    "ticker",
    "timestamp",
    "open_price",
    "high_price",
    "low_price",
    "close_price",
    "volume",
    "volume_weighted_average_price",
    "sector",
    "industry",
)

filtered_data = consolidated_data.select(retained_columns)

filtered_data.write_csv(output_csv_path)
