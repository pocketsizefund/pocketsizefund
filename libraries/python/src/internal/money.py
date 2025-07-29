from decimal import ROUND_HALF_UP, Decimal

from pydantic import BaseModel, Field, field_validator


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

    def to_dict(self) -> dict[str, float]:
        return {"amount": float(self.amount)}
