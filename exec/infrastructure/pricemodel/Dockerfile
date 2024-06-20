ARG PYTHON_VERSION=3.9.19
FROM python:${PYTHON_VERSION} as base

ARG MODEL_FILE_NAME

ENV PYTHONDONTWRITEBYTECODE=1
ENV MODEL_FILE_NAME=$MODEL_FILE_NAME
ENV DARQUBE_API_KEY=""
ENV ALPACA_API_KEY=""
ENV ALPACA_API_SECRET=""
ENV ALPHA_VANTAGE_API_KEY=""
ENV EDGAR_USER_AGENT=""

WORKDIR /app

ARG UID=10001
RUN adduser \
    --disabled-password \
    --gecos "" \
    --home "/nonexistent" \
    --shell "/sbin/nologin" \
    --no-create-home \
    --uid "${UID}" \
    appuser


COPY pyproject.toml poetry.lock .

RUN pip install --no-cache-dir "poetry==1.7.1" && \
  poetry config virtualenvs.create false && \
  poetry install --no-dev --no-interaction --no-cache --no-root


USER appuser

COPY entrypoint.py .

EXPOSE 8080

ENTRYPOINT ["python", "entrypoint.py"]
