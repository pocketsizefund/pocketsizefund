ARG PYTHON_VERSION=3.10.14
FROM python:${PYTHON_VERSION} AS base
COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

ENV PYTHONDONTWRITEBYTECODE=1
WORKDIR /app

COPY pyproject.toml uv.lock /app

COPY src /app/src

RUN uv sync

RUN cd /app/src

EXPOSE 8080

ENTRYPOINT ["uv", "run", "uvicorn", "src.positionmanager.entrypoint:app", "--host", "0.0.0.0", "--port", "8080"]
