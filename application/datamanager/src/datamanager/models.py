import datetime
from pydantic import BaseModel, Field, field_validator
from pydantic_core import core_schema


class SummaryDate(BaseModel):
    date: datetime.date = Field(
        default_factory=lambda: datetime.datetime.now(tz=datetime.timezone.utc).date(),
    )

    @field_validator("date", mode="before")
    def parse_date(cls, value: datetime.date | str) -> datetime.date:
        if isinstance(value, datetime.date):
            return value
        for fmt in ("%Y-%m-%d", "%Y/%m/%d"):
            try:
                return (
                    datetime.datetime.strptime(value, fmt)
                    .replace(tzinfo=datetime.timezone.utc)
                    .date()
                )
            except ValueError:
                continue
        msg = "Invalid date format: expected YYYY-MM-DD or YYYY/MM/DD"
        raise ValueError(msg)

    model_config = {"json_encoders": {datetime.date: lambda d: d.strftime("%Y/%m/%d")}}


class DateRange(BaseModel):
    start: datetime.date
    end: datetime.date

    @field_validator("end")
    @classmethod
    def check_end_after_start(
        cls,
        end_value: datetime.datetime,
        info: core_schema.ValidationInfo,
    ) -> datetime.datetime:
        start_value = info.data.get("start")
        if start_value and end_value <= start_value:
            msg = "End date must be after start date."
            raise ValueError(msg)
        return end_value


class BarsSummary(BaseModel):
    date: str
    count: int
