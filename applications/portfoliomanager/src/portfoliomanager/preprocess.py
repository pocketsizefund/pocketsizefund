import polars as pl


def filter_equity_bars(
    data: pl.DataFrame,
    minimum_average_close_price: float = 10.0,
    minimum_average_volume: float = 1_000_000.0,
) -> pl.DataFrame:
    data = data.clone()

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
