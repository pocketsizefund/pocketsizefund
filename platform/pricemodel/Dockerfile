ARG PYTHON_VERSION=3.10.14
FROM python:${PYTHON_VERSION} AS base

ADD --chmod=755 https://astral.sh/uv/install.sh /install.sh
RUN /install.sh && rm /install.sh

ARG MODEL_FILE_NAME

ENV PYTHONDONTWRITEBYTECODE=1
ENV MODEL_FILE_NAME=$MODEL_FILE_NAME
ENV DARQUBE_API_KEY=""
ENV ALPACA_API_KEY=""
ENV ALPACA_API_SECRET=""
ENV ALPHA_VANTAGE_API_KEY=""
ENV EDGAR_USER_AGENT=""
ENV SENTRY_DSN=""

WORKDIR /app

ARG UID=10001
RUN adduser \
    --disabled-password \
    --gecos "" \
    --home "/nonexistent" \
    --shell "/sbin/nologin" \
    --no-create-home \
    --uid "${UID}" \
    appuser

COPY requirements.txt .

RUN /root/.cargo/bin/uv pip install --system --no-cache -r requirements.txt
RUN mkdir /app/lightning_logs && chmod 777 /app/lightning_logs

USER appuser

COPY entrypoint.py . price-model.ckpt ./

ENV MPLCONFIGDIR=/tmp

EXPOSE 8080

ENTRYPOINT ["uvicorn", "entrypoint:app", "--host", "0.0.0.0", "--port", "8080"]
