from prefect import flow, task
import requests
import instructor
from openai import OpenAI
import tempfile

from exec.pipeline.financials.structs import FinancialStatement, EarningsStatement

import PyPDF2
from bs4 import BeautifulSoup

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
def read_webpage(url) -> str:
    response = requests.get(url)
    response.raise_for_status()

    soup = BeautifulSoup(response.text, 'html.parser')
    body = soup.find("body").get_text(strip=True)

    client = instructor.from_openai(OpenAI())
    return client.chat.completions.create(
        model="gpt-3.5-turbo-16k",
        response_model=str,
        messages=[{"role": "system", 
                   "content": "You are reading a webpage and returning only the content, ignoring all HTML/JS in it"},
                  {"role": "user", "content": body}],
    )


@task
def parse_financial_statements(data: str) -> FinancialStatement:
    client = instructor.from_openai(OpenAI())

    return client.chat.completions.create(
        model="gpt-3.5-turbo-16k",
        response_model=FinancialStatement,
        messages=[{"role": "system", 
                   "content": "You are extracting data from a public financial document and structuring its output"},
                  {"role": "user", "content": data}],
    )


@task
def parse_earnings_statement(data: str) -> EarningsStatement:
    client = instructor.from_openai(OpenAI())

    return client.chat.completions.create(
        model="gpt-3.5-turbo-16k",
        response_model=EarningsStatement,
        messages=[{"role": "system", 
                   "content": "You are extracting the sentiment from a quarterly earnings statement"},
                  {"role": "user", "content": data}],
    )

@flow
def pull_financial_statements(
        financial_statement_url: str,
        earnings_statement_url: str):
    financial_statements = read_remote_pdf(financial_statement_url)
    quarterly_earnings_statement = read_webpage(earnings_statement_url)

    parsed_financial_statements = parse_financial_statements(financial_statements)
    earnings_statement = parse_earnings_statement(quarterly_earnings_statement)

    print(parsed_financial_statements)
    print(earnings_statement)

if __name__ == "__main__":
    financial_statement_url = "https://www.apple.com/newsroom/pdfs/fy2024-q2/FY24_Q2_Consolidated_Financial_Statements.pdf"
    earnings_statement_url = "https://www.apple.com/newsroom/2024/05/apple-reports-second-quarter-results/"

    pull_financial_statements(financial_statement_url, earnings_statement_url)
