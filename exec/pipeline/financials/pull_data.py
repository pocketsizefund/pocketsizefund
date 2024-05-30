from prefect import flow, task
from pydantic import BaseModel
import requests
import instructor
from openai import OpenAI
import tempfile
from decimal import Decimal

import PyPDF2


class CurrentAssets(BaseModel):
    cash_and_cash_equivalents: Decimal
    marketable_securities: Decimal
    accounts_receivable_net: Decimal
    vendor_non_trade_receivables: Decimal
    other_current_assets: Decimal
    total_current_assets: Decimal


class FinancialStatement(BaseModel):
    current_assets: CurrentAssets


@task
def read_remote_pdf(url) -> str:
    response = requests.get(url)

    with tempfile.NamedTemporaryFile(mode="wb", delete=False) as f:
        f.write(response.content)
        reader = PyPDF2.PdfReader(f.name)

    text = ""
    for page in reader.pages:
        text += page.extract_text()

    return text


@task
def parse_financial_statements(data: str) -> FinancialStatement:
    client = instructor.from_openai(OpenAI())

    return client.chat.completions.create(
        model="gpt-3.5-turbo",
        response_model=FinancialStatement,
        messages=[{"role": "system", 
                   "content": "You are extracting data from a public financial document and structuring its output"},
                  {"role": "user", "content": data}],
    )


@flow
def pull_financial_statements(url: str):
    financial_statements = read_remote_pdf(url)
    parsed_financial_statements = parse_financial_statements(financial_statements)
    print(parsed_financial_statements)

if __name__ == "__main__":
    url = "https://www.apple.com/newsroom/pdfs/fy2024-q2/FY24_Q2_Consolidated_Financial_Statements.pdf"
    pull_financial_statements(url)
