from pydantic import BaseModel, Field, conint

ColumnSubset = list[str]


class Bucket(BaseModel):
    block: str = Field(..., description="Name of the prefect block, not the bucket name itself.")
    prefix: str
    key: str


class TimeWindow(BaseModel):
    input: conint(ge=1)
    output: conint(ge=1)

    @property
    def length(self) -> int:
        return self.input + self.output
