FROM python:3.12.10

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

ENV PYTHONPATH=/app/src

WORKDIR /app

COPY pyproject.toml ./

RUN uv sync --no-dev

COPY ./src ./src

EXPOSE 8080

ENTRYPOINT ["uv", "run", "uvicorn", "datamanager.main:application", "--host", "0.0.0.0", "--port", "8080", "--app-dir", "src"]
