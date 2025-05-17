FROM python:3.13-slim 

COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

WORKDIR /app

COPY pyproject.toml ./

COPY application/positionmanager/ ./application/positionmanager/

RUN uv sync --all-packages

EXPOSE 8080

ENTRYPOINT ["uv", "run", "uvicorn", "application.positionmanager.src.positionmanager.main:application", "--host", "0.0.0.0", "--port", "8080"]