FROM python:3.13-slim 

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

RUN apt-get update && \
    apt-get install -y --no-install-recommends libgomp1 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY pyproject.toml ./

COPY application/datamanager/ ./application/datamanager/

RUN uv sync --no-dev

EXPOSE 8000

ENTRYPOINT ["uv", "run", "uvicorn", "application.datamanager.src.datamanager.main:application", "--host", "0.0.0.0", "--port", "8080"]
