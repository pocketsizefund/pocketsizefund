from datetime import date

from pydantic import BaseModel, Field, field_validator


class EquityBar(BaseModel):
    ticker: str = Field(..., description="The stock symbol")
    timestamp: date = Field(..., description="The date of the bar in YYYY-MM-DD format")
    open_price: float = Field(..., description="The opening price")
    high_price: float = Field(..., description="The highest price")
    low_price: float = Field(..., description="The lowest price")
    close_price: float = Field(..., description="The closing price")
    volume: float = Field(..., description="The trading volume")
    volume_weighted_average_price: float = Field(
        ..., description="The volume-weighted average price"
    )

    @field_validator("ticker", mode="before")
    @classmethod
    def validate_ticker(cls, value: str) -> str:
        if not value or not value.strip():
            message = "Ticker cannot be empty."
            raise ValueError(message)
        return value.strip().upper()

    @field_validator(
        "open_price",
        "high_price",
        "low_price",
        "close_price",
        mode="before",
    )
    @classmethod
    def validate_prices(cls, value: float) -> float:
        if value < 0:
            message = "Price cannot be negative."
            raise ValueError(message)
        return value

    @field_validator("timestamp", mode="before")
    @classmethod
    def validate_timestamp(cls, value: date | str) -> date:
        if isinstance(value, date):
            return value
        try:
            return date.fromisoformat(value)
        except ValueError as e:
            message = "Invalid date format: expected YYYY-MM-DD"
            raise ValueError(message) from e
