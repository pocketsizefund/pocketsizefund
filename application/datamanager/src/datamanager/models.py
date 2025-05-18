from datetime import date
from pydantic import BaseModel, field_validator


class DateRange(BaseModel):
    start: date
    end: date

    @field_validator("end")
    @classmethod
    def check_end_after_start(cls, end_value, info):
        start_value = info.data.get("start")
        if start_value and end_value <= start_value:
            raise ValueError("End date must be after start date.")
        return end_value


class BarsResult(BaseModel):
    date: str
    count: int
