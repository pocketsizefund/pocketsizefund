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
            average_close_price=pl.col("close_price").mean(),
            average_volume=pl.col("volume").mean(),
        )
        .filter(
            (pl.col("average_close_price") > minimum_average_close_price)
            & (pl.col("average_volume") > minimum_average_volume)
        )
        .drop(["average_close_price", "average_volume"])
    )
