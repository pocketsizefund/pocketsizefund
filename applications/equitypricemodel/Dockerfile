# training

FROM python:3.12.10-slim AS trainer-builder

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv 

WORKDIR /app

COPY pyproject.toml uv.lock ./

COPY applications/equitypricemodel/ applications/equitypricemodel/

RUN uv sync --no-dev --no-cache 

FROM python:3.12.10-slim AS trainer-runner

RUN apt-get update && \
    apt-get install -y --no-install-recommends build-essential clang && \
    rm -rf /var/lib/apt/lists/*

ENV CPU=1

WORKDIR /app

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

COPY --from=trainer-builder /app /app

ENV PYTHONPATH=/app/applications/equitypricemodel/src

ENTRYPOINT ["uv", "run", "python", "applications/equitypricemodel/src/equitypricemodel/trainer.py"]

# inference

FROM python:3.12.10-slim AS server-builder


