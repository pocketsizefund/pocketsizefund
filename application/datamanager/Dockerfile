FROM python:3.13-slim 
COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

RUN apt-get update && \
    apt-get install -y --no-install-recommends libgomp1 curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY pyproject.toml ./
RUN uv sync --no-dev

COPY ./src/ ./app

EXPOSE 8080

ENTRYPOINT ["uv", "run", "uvicorn", "datamanager.src.datamanager.main:application", "--host", "0.0.0.0", "--port", "8080"]
