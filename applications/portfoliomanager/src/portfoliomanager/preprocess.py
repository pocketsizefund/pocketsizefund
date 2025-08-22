import polars as pl


def filter_equity_bars(data: pl.DataFrame) -> pl.DataFrame:
    data = data.clone()

    minimum_average_close_price = 10.0
    minimum_average_volume = 1_000_000.0

    return (
        data.group_by("ticker")
        .agg(
            avg_close_price=pl.col("close_price").mean(),
            avg_volume=pl.col("volume").mean(),
        )
        .filter(
            (pl.col("avg_close_price") > minimum_average_close_price)
            & (pl.col("avg_volume") > minimum_average_volume)
        )
    )
