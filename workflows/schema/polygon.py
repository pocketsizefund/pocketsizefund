from typing import Optional
from pandera.polars import PolarsData
import pandera.polars as pa
from pandera.typing.polars import DataFrame, Series
import polars as pl


class RawTickers(pa.DataFrameModel):
    ticker: Series[str] = pa.Field()
    name: Series[str] = pa.Field()
    cik: Series[str] = pa.Field(nullable=True)
    type: Series[str] = pa.Field(nullable=True)
    active: Series[bool] = pa.Field()
    market: Series[str] = pa.Field()
    primary_exchange: Series[str] = pa.Field(nullable=True)
    currency_symbol: Series[str] = pa.Field(nullable=True)
    currency_name: Series[str] = pa.Field(nullable=True)
    base_currency_symbol: Series[str] = pa.Field(nullable=True)
    base_currency_name: Series[str] = pa.Field(nullable=True)
    delisted_utc: Series[str] = pa.Field(nullable=True)
    composite_figi: Series[str] = pa.Field(nullable=True)
    share_class_figi: Series[str] = pa.Field(nullable=True)
    locale: Series[str] = pa.Field(nullable=True)
    last_updated_utc: Series[str] = pa.Field(nullable=True)


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


class Bars(pa.DataFrameModel):
    ticker: Series[str] = pa.Field()
    open: Series[float] = pa.Field(ge=0)
    high: Series[float] = pa.Field(ge=0)
    low: Series[float] = pa.Field(ge=0)
    close: Series[float] = pa.Field(ge=0)
    volume: Series[float] = pa.Field(ge=0)
    # date: Series[datetime] = pa.Field()

    @pa.dataframe_check
    def high_price_greatest_price(cls, data: PolarsData) -> DataFrame:
        """Ensure that the daily high price is higher than open/close/low."""
        return data.lazyframe.select(
            pl.col("high").ge(pl.col("low"))
            & pl.col("high").ge(pl.col("open"))
            & pl.col("high").ge(pl.col("close"))
        )


class BarTrainingData(pa.DataFrameModel):
    ticker: Series[str] = pa.Field()
    open: Series[float] = pa.Field(ge=0)
    high: Series[float] = pa.Field(ge=0)
    low: Series[float] = pa.Field(ge=0)
    close: Series[float] = pa.Field(ge=0)
    volume: Series[float] = pa.Field(ge=0)
    # date: Series[datetime] = pa.Field()

    @pa.dataframe_check
    def high_price_greatest_price(cls, data: PolarsData) -> DataFrame:
        """Ensure that the daily high price is higher than open/close/low."""
        return data.lazyframe.select(
            pl.col("high").ge(pl.col("low"))
            & pl.col("high").ge(pl.col("open"))
            & pl.col("high").ge(pl.col("close"))
        )
