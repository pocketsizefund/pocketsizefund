import sys

import polars as pl
from loguru import logger

from .categories_schema import categories_schema


def combine_data(
    categories_csv_path: str,
    equity_bars_csv_path: str,
    output_csv_path: str,
) -> None:
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


if __name__ == "__main__":
    if len(sys.argv) != 4:  # noqa: PLR2004
        logger.error(
            "Usage: python -m equitypricemodel.combine_data <categories_csv> <equity_bars_csv> <output_csv>"  # noqa: E501
        )
        sys.exit(1)

    combine_data(sys.argv[1], sys.argv[2], sys.argv[3])
