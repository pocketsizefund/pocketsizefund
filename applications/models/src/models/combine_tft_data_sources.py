import os
from datetime import datetime
from zoneinfo import ZoneInfo

import polars as pl
from internal.company_information import company_information_schema
from internal.equity_bar import equity_bar_schema

filepath = "applications/models/src/models/"

categories_path = filepath + "categories.csv"
equity_bars_path = filepath + "equity_bars.csv"

if not os.path.exists(categories_path):  # noqa: PTH110
    message = f"Required file not found: {categories_path}"
    raise FileNotFoundError(message)
if not os.path.exists(equity_bars_path):  # noqa: PTH110
    message = f"Required file not found: {equity_bars_path}"
    raise FileNotFoundError(message)

categories = company_information_schema.validate(pl.read_csv(categories_path))
equity_bars = equity_bar_schema.validate(pl.read_csv(equity_bars_path))


combined_data = equity_bars.join(
    categories.select(["ticker", "sector", "industry"]), on="ticker", how="left"
).select(
    [
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
    ]
)

timestamp = datetime.now(tz=ZoneInfo("America/New_York")).strftime("%Y%m%d_%H%M%S")
output_filename = f"{filepath}training_data_{timestamp}.csv"
combined_data.write_csv(output_filename)
