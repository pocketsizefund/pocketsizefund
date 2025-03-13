ARG PYTHON_VERSION=3.10.14

FROM python:${PYTHON_VERSION} AS base

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

WORKDIR /app

COPY pyproject.toml uv.lock /app

COPY *.py /app/

COPY miniature_temporal_fusion_transformer.safetensor /app/

RUN uv sync

EXPOSE 8080

ENTRYPOINT ["uv", "run", "uvicorn", "pricemodel.main:app", "--host", "0.0.0.0", "--port", "8080"]
