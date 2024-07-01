"""Creates the basic data models for the financial pipeline."""
from typing import Literal

from pydantic import AnyUrl, BaseModel


class StatementConfig(BaseModel):
    """Financial statement file configuration."""

    url: AnyUrl
    filetype: Literal["pdf", "html"]


class Config(BaseModel):
    """Configuration for financial statements."""

    financial_statement: StatementConfig
    earnings_statement: StatementConfig



