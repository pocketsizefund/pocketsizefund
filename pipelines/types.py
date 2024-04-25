from pydantic import BaseModel, conint

ColumnSubset = list[str]


class TimeWindow(BaseModel):
    input: conint(ge=1)
    output: conint(ge=1)

    @property
    def length(self):
        return self.input + self.output
