ARG PYTHON_VERSION=3.10.14

FROM python:${PYTHON_VERSION} AS base

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

WORKDIR /app

COPY pyproject.toml uv.lock /app

COPY main.py /app/main.py

RUN uv sync

EXPOSE 8080

ENTRYPOINT ["uv", "run", "uvicorn", "positionmanager.main:app", "--host", "0.0.0.0", "--port", "8080"]
