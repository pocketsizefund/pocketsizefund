ARG PYTHON_VERSION=3.10.14
FROM python:${PYTHON_VERSION} AS base
COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

ENV PYTHONDONTWRITEBYTECODE=1

WORKDIR /app

RUN mkdir /app/lightning_logs && chmod 777 /app/lightning_logs

COPY pyproject.toml uv.lock /app/
COPY src/pricemodel/__init__.py /app/src/pricemodel/__init__.py

RUN uv sync

COPY src/ /app/src

FROM base AS training

ENV MPLCONFIGDIR=/tmp

CMD ["uv", "run", "python", "-m", "pricemodel.pipeline.daily.tft2"]

FROM base AS serving
COPY src /app/src
COPY price-model.ckpt /app/price-model.ckpt

EXPOSE 8080

CMD ["uv", "run", "uvicorn", "pricemodel.entrypoint:app", "--host", "0.0.0.0", "--port", "8080"]
