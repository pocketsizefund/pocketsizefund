"""Runtime data structures for financial statements."""

from decimal import Decimal

from pydantic import BaseModel, Field


class NetSales(BaseModel):
    products: Decimal
    services: Decimal
    total_net_sales: Decimal


class CostofSales(BaseModel):
    products: Decimal
    services: Decimal
    total_cost_of_sales: Decimal


class OperatingExpenses(BaseModel):
    research_and_development: Decimal
    selling_general_and_administrative: Decimal
    total_operating_expenses: Decimal


class CurrentAssets(BaseModel):
    cash_and_cash_equivalents: Decimal
    marketable_securities: Decimal
    accounts_receivable_net: Decimal
    vendor_non_trade_receivables: Decimal
    other_current_assets: Decimal
    total_current_assets: Decimal


class NonCurrentAssets(BaseModel):
    marketable_securities: Decimal
    property_plant_and_equipment_net: Decimal
    other_non_current_assets: Decimal
    total_non_current_assets: Decimal


class Assets(BaseModel):
    current_assets: CurrentAssets
    non_current_assets: NonCurrentAssets


class EarningsPerShare(BaseModel):
    basic: Decimal
    diluted: Decimal


class Shares(BaseModel):
    basic: int
    diluted: int


class NetSalesByReportableSegment(BaseModel):
    americas: Decimal
    europe: Decimal
    greater_china: Decimal
    japan: Decimal
    rest_of_asia_pacific: Decimal
    total_net_sales: Decimal


class NetSalesByCategory(BaseModel):
    iphone: Decimal
    mac: Decimal
    ipad: Decimal
    wearable_home_and_accessories: Decimal
    services: Decimal
    total_net_sales: Decimal


class CurrentLiabilities(BaseModel):
    accounts_payable: Decimal
    other_current_liabilities: Decimal
    defeerred_revenue: Decimal
    commercial_paper: Decimal
    term_debt: Decimal
    total_current_liabilities: Decimal


class NonCurrentLiabilities(BaseModel):
    term_debt: Decimal
    other_non_current_liabilities: Decimal
    total_non_current_liabilities: Decimal


class Liabilities(BaseModel):
    current_liabilities: CurrentLiabilities
    non_current_liabilities: NonCurrentLiabilities
    total_liabilities: Decimal


class FinancialStatement(BaseModel):
    time_window: str
    net_sales: NetSales
    cost_of_sales: CostofSales
    gross_margin: Decimal
    operating_expenses: OperatingExpenses
    operating_income: Decimal
    other_income_net: Decimal
    income_before_provision_for_taxes: Decimal
    provision_for_taxes: Decimal
    net_income: Decimal
    earnings_per_share: EarningsPerShare
    shares_used_in_computing_earnings_per_share: Shares
    assets: Assets
    liabilities: Liabilities
    commitmements_and_contingencies: Decimal


class EarningsStatement(BaseModel):
    confidence: int = Field(
        ...,
        description="Confidence: Indicates strong belief in future performance.",
    )
    optimism: int = Field(..., description="Optimism: Shows positive outlook and expectations.")
    satisfaction: int = Field(
        ...,
        description="Satisfaction: Reflects contentment with current performance.",
    )
    excitement: int = Field(
        ...,
        description="Excitement: Demonstrates enthusiasm about recent achievements or future prospects.",
    )
    concern: int = Field(
        ...,
        description="Concern: Indicates worry about potential issues or risks.",
    )
    uncertainty: int = Field(
        ...,
        description="Uncertainty: Reflects lack of clarity about future outcomes.",
    )
    disappointment: int = Field(
        ...,
        description="Disappointment: Shows dissatisfaction with current or past performance.",
    )
    pessimism: int = Field(
        ...,
        description="Pessimism: Demonstrates a negative outlook and expectations.",
    )
    cautious_optimism: int = Field(
        ...,
        description="Cautious Optimism: Combination of hopefulness and caution.",
    )
    neutrality: int = Field(
        ...,
        description="Neutrality: Balanced view without strong positive or negative bias.",
    )
    mixed_emotions: int = Field(
        ...,
        description="Mixed Emotions: Conflicting feelings about different aspects of performance.",
    )
    trust: int = Field(
        ...,
        description="Trust: Confidence in the management team and their decisions.",
    )
    anticipation: int = Field(
        ...,
        description="Anticipation: Looking forward to upcoming events or developments.",
    )
    regret: int = Field(
        ...,
        description="Regret: Expressing remorse over past decisions or performance.",
    )
    relief: int = Field(
        ...,
        description="Relief: Feeling of alleviation after overcoming challenges.",
    )
