from pydantic import BaseModel, AnyUrl
from typing import Literal

class StatementConfig(BaseModel):
    url: AnyUrl
    filetype: Literal["pdf", "html"]


class Config(BaseModel):
    financial_statement: StatementConfig
    earnings_statement: StatementConfig



