"""Configuration for the financials pipeline."""
from typing import Literal

from pydantic import AnyUrl, BaseModel


class StatementConfig(BaseModel):
    url: AnyUrl
    filetype: Literal["pdf", "html"]


class Config(BaseModel):
    financial_statement: StatementConfig
    earnings_statement: StatementConfig



