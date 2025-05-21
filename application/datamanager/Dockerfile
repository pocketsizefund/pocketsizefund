FROM python:3.13
COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

WORKDIR /app

COPY pyproject.toml ./
RUN uv sync --no-dev

COPY ./src/ ./src/

EXPOSE 8080

ENTRYPOINT ["uv", "run", "uvicorn", "datamanager.src.datamanager.main:application", "--host", "0.0.0.0", "--port", "8080"]
