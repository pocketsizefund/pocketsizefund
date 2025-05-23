import datetime
from pydantic import BaseModel, Field, field_validator


class SummaryDate(BaseModel):
    date: datetime.date = Field(
        default_factory=lambda: datetime.datetime.utcnow().date()
    )

    @field_validator("date", mode="before")
    def parse_date(cls, value):
        if isinstance(value, datetime.date):
            return value
        for fmt in ("%Y-%m-%d", "%Y/%m/%d"):
            try:
                return datetime.datetime.strptime(value, fmt).date()
            except ValueError:
                continue
        raise ValueError("Invalid date format: expected YYYY-MM-DD or YYYY/MM/DD")

    model_config = {"json_encoders": {datetime.date: lambda d: d.strftime("%Y/%m/%d")}}


class DateRange(BaseModel):
    start: datetime.date
    end: datetime.date

    @field_validator("end")
    @classmethod
    def check_end_after_start(cls, end_value, info):
        start_value = info.data.get("start")
        if start_value and end_value <= start_value:
            raise ValueError("End date must be after start date.")
        return end_value


class BarsSummary(BaseModel):
    date: str
    count: int
