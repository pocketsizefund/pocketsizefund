FROM python:3.12.10-slim AS builder 

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv  

WORKDIR /app

COPY pyproject.toml uv.lock ./

COPY applications/equitypricemodel/ applications/equitypricemodel/

RUN uv sync --no-dev --no-cache 

FROM python:3.12.10-slim AS trainer

RUN apt-get update && \
    apt-get install -y --no-install-recommends build-essential clang && \
    rm -rf /var/lib/apt/lists/* 

ENV CPU=1

WORKDIR /app

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv 

COPY --from=builder /app /app

ENV PYTHONPATH=/app/applications/equitypricemodel/src

ENTRYPOINT ["uv", "run", "python", "applications/equitypricemodel/src/equitypricemodel/trainer.py"]

FROM python:3.12.10-slim AS server

RUN apt-get update && \
    apt-get install -y --no-install-recommends build-essential clang && \
    rm -rf /var/lib/apt/lists/*

ENV PYTHONPATH=/app/applications/equitypricemodel/src

WORKDIR /app

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

COPY --from=builder /app /app

COPY applications/equitypricemodel/src/equitypricemodel/tide_states.safetensor app/tide_states.safetensor

COPY applications/equitypricemodel/src/equitypricemodel/tide_parameters.json app/tide_parameters.json

ENTRYPOINT ["uv", "run", "python", "applications/equitypricemodel/src/equitypricemodel/server.py"]

