from datetime import datetime
from decimal import ROUND_HALF_UP, Decimal
from typing import Any, Dict

from pydantic import BaseModel, Field, field_validator
from pydantic_core import core_schema


class Money(BaseModel):
    amount: Decimal = Field(
        ...,
        description="Monetary value in USD with 2 decimal places",
    )

    @field_validator("amount", check_fields=True)
    def validate_amount(cls, v: str | Decimal) -> Decimal:  # noqa: N805
        if not isinstance(v, Decimal):
            v = Decimal(str(v))

        return v.quantize(Decimal("0.01"), rounding=ROUND_HALF_UP)

    def __float__(self) -> float:
        return float(self.amount)

    def __str__(self) -> str:
        return f"${self.amount:.2f}"

    def __repr__(self) -> str:
        return f"Money(amount=Decimal('{self.amount:.2f}'))"

    @classmethod
    def from_float(cls, value: float) -> "Money":
        return cls(amount=Decimal(str(value)))

    def to_dict(self) -> Dict[str, float]:
        return {"amount": float(self.amount)}


class DateRange(BaseModel):
    start: datetime
    end: datetime

    @field_validator("end")
    @classmethod
    def check_end_after_start(
        cls,
        end_value: datetime,
        info: core_schema.ValidationInfo,
    ) -> datetime:
        start_value = info.data.get("start")
        if start_value and end_value <= start_value:
            msg = "End date must be after start date."
            raise ValueError(msg)

        return end_value

    def to_payload(self) -> Dict[str, str]:
        return {
            "start_date": self.start.isoformat(),
            "end_date": self.end.isoformat(),
        }


class PredictionPayload(BaseModel):
    predictions: Dict[str, Any]
