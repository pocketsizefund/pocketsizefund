FROM python:3.10.14

ENV POETRY_VERSION=1.7.1 POETRY_VIRTUALENVS_CREATE=false
ENV DARQUBE_API_KEY=""
ENV ALPACA_API_KEY=""
ENV ALPACA_API_SECRET=""
ENV ALPHA_VANTAGE_API_KEY=""
ENV MODEL_ENDPOINT_NAME=""
ENV IS_PAPER=""

LABEL app=hermes

COPY pyproject.toml poetry.lock ./

RUN pip install --no-cache-dir "poetry==1.7.1" && \
  poetry install --no-dev --no-interaction --no-cache

COPY entrypoint.py .

CMD [ "python", "entrypoint.py" ]
