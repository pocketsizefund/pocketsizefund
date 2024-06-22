"""Pipeline to pull financial statements from a website."""

import tempfile

import instructor
import PyPDF2
import requests
from bs4 import BeautifulSoup
from openai import OpenAI
from prefect import flow, task
from prefect_ray import RayTaskRunner

from pipelines.filings.config import Config, StatementConfig
from pipelines.filings.structs import EarningsStatement, FinancialStatement


@task
def read_remote_pdf(url: str) -> str:
    """Read a remote PDF file and return the text."""
    response = requests.get(url)

    with tempfile.NamedTemporaryFile(mode="wb", delete=False) as f:
        f.write(response.content)
        reader = PyPDF2.PdfReader(f.name)

    text = ""
    for page in reader.pages:
        text += page.extract_text()

    return text


@task
def read_html(url: str) -> str:
    """Read a remote HTML file and return the text."""
    response = requests.get(url)
    response.raise_for_status()

    soup = BeautifulSoup(response.text, "html.parser")
    body = soup.find("body").get_text(strip=True)

    client = instructor.from_openai(OpenAI())
    return client.chat.completions.create(
        model="gpt-3.5-turbo-16k",
        response_model=str,
        messages=[
            {
                "role": "system",
                "content": "You are reading a webpage and returning only the content, ignoring all HTML/JS in it",
            },
            {"role": "user", "content": body},
        ],
    )


@task
def parse_financial_statements(data: str) -> FinancialStatement:
    """Parse a financial statement from a string."""
    client = instructor.from_openai(OpenAI())

    return client.chat.completions.create(
        model="gpt-3.5-turbo-16k",
        response_model=FinancialStatement,
        messages=[
            {
                "role": "system",
                "content": "You are extracting data from a public financial document and structuring its output",
            },
            {"role": "user", "content": data},
        ],
    )


@task
def parse_earnings_statement(data: str) -> EarningsStatement:
    """Parse an earnings statement from a string."""
    client = instructor.from_openai(OpenAI())

    return client.chat.completions.create(
        model="gpt-3.5-turbo-16k",
        response_model=EarningsStatement,
        messages=[
            {
                "role": "system",
                "content": "You are extracting the sentiment from a quarterly earnings statement",
            },
            {"role": "user", "content": data},
        ],
    )


@flow
def pull_financial_statements(config: Config) -> None:
    """Pull financial statements from a website."""
    if config.financial_statement.filetype == "pdf":
        financial_statements = read_remote_pdf(config.financial_statement.url)
    elif config.financial_statement.filetype == "html":
        financial_statements = read_html(config.financial_statement.url)

    if config.earnings_statement.filetype == "pdf":
        quarterly_earnings_statement = read_remote_pdf(config.earnings_statement.url)
    elif config.earnings_statement.filetype == "html":
        quarterly_earnings_statement = read_html(config.earnings_statement.url)

    parse_financial_statements(financial_statements)
    parse_earnings_statement(quarterly_earnings_statement)


@flow(task_runner=RayTaskRunner())
def statements_pipeline(configs: list[Config]) -> None:
    """Pipeline to pull financial statements from a website."""
    for config in configs:
        pull_financial_statements(config)


if __name__ == "__main__":
    apple = Config(
        financial_statement=StatementConfig(
            url="https://www.apple.com/newsroom/pdfs/fy2024-q2/FY24_Q2_Consolidated_Financial_Statements.pdf",
            filetype="pdf",
        ),
        earnings_statement=StatementConfig(
            url="https://www.apple.com/newsroom/2024/05/apple-reports-second-quarter-results/",
            filetype="html",
        ),
    )

    amazon = Config(
        financial_statement=StatementConfig(
            url="https://s2.q4cdn.com/299287126/files/doc_financials/2024/q1/AMZN-Q1-2024-Earnings-Release.pdf",
            filetype="pdf",
        ),
        earnings_statement=StatementConfig(
            url="https://ir.aboutamazon.com/news-release/news-release-details/2024/Amazon.com-Announces-First-Quarter-Results-68b9258cd/",
            filetype="html",
        ),
    )

    exxon_mobil = Config(
        financial_statement=StatementConfig(
            url="https://d1io3yog0oux5.cloudfront.net/_2015997b5f161254d9ac4d3d42a6ccb7/exxonmobil/db/2288/22249/earnings_release/1Q24+Earnings+Press+Release+Website.pdf",
            filetype="pdf",
        ),
        earnings_statement=StatementConfig(
            url="https://d1io3yog0oux5.cloudfront.net/_2015997b5f161254d9ac4d3d42a6ccb7/exxonmobil/db/2288/22249/webcast_transcript/1Q24+Earnings+Call+Transcript_Final.pdf",
            filetype="pdf",
        ),
    )

    statements_pipeline([exxon_mobil])
