from typing import Optional
from pandera.polars import PolarsData
import pandera.polars as pa
from pandera.typing.polars import DataFrame, Series
import polars as pl


class RawBars(pa.DataFrameModel):
    ticker: Optional[Series[str]] = pa.Field()
    open: Optional[Series[float]] = pa.Field(ge=0)
    high: Optional[Series[float]] = pa.Field(ge=0)
    low: Optional[Series[float]] = pa.Field(ge=0)
    close: Optional[Series[float]] = pa.Field(ge=0)
    volume: Optional[Series[float]] = pa.Field(ge=0)
    vwap: Optional[Series[float]] = pa.Field(nullable=True)
    timestamp: Optional[Series[int]] = pa.Field(ge=0)

    @pa.dataframe_check
    def high_price_greatest_price(cls, data: PolarsData) -> DataFrame:
        """Ensure that the daily high price is higher than open/close/low."""
        return data.lazyframe.select(
            pl.col("high").ge(pl.col("low"))
            & pl.col("high").ge(pl.col("open"))
            & pl.col("high").ge(pl.col("close"))
        )
