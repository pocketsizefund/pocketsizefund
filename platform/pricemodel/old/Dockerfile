ARG PYTHON_VERSION=3.10.14

FROM python:${PYTHON_VERSION} AS base

# COPY --from=ghcr.io/astral-sh/uv:latest /uv /bin/uv

# WORKDIR /app

# COPY pyproject.toml uv.lock /app

# ARG MODEL_FILE_NAME

# ENV PYTHONDONTWRITEBYTECODE=1

# COPY . /app/

# RUN uv sync

# RUN mkdir /app/lightning_logs && chmod 777 /app/lightning_logs

# COPY price-model.ckpt ./

# ENV MPLCONFIGDIR=/tmp

# EXPOSE 8080

# ENTRYPOINT ["uv", "run", "uvicorn", "pricemodel.entrypoint:app", "--host", "0.0.0.0", "--port", "8080"]
