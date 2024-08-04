ARG RUST_VERSION=1.79.0
ARG APP_NAME=discord

FROM lukemathwalker/cargo-chef:latest-rust-${RUST_VERSION} AS chef
ARG APP_NAME
WORKDIR /app

FROM chef AS planner
COPY . .

RUN cargo chef prepare  --recipe-path recipe.json

FROM chef AS builder
COPY --from=planner /app/recipe.json recipe.json

RUN cargo chef cook --release --recipe-path recipe.json
COPY . .

ENV SQLX_OFFLINE=true
ENV APP_NAME=${APP_NAME}

RUN cargo build --release --bin chronos

FROM debian:bookworm-slim AS final
WORKDIR /app
RUN apt-get update -y \
    && apt-get install -y --no-install-recommends openssl ca-certificates \
    # Clean up
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/chronos chronos

ENTRYPOINT ["./chronos"]
