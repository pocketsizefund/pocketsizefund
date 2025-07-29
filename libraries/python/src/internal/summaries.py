from pydantic import BaseModel


class BarsSummary(BaseModel):
    date: str
    count: int
