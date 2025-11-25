FROM lukemathwalker/cargo-chef:0.1.72-rust-1.89.0-slim AS chef

RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config \
    libssl-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

RUN rustup toolchain install nightly

RUN rustup default nightly

WORKDIR /app

FROM chef AS planner

COPY applications/datamanager/ applications/datamanager/

COPY Cargo.toml Cargo.lock ./ 

COPY applications/datamanager/Cargo.toml ./applications/datamanager/Cargo.toml

RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder

COPY --from=planner /app/recipe.json recipe.json

RUN cargo chef cook --release --recipe-path recipe.json

COPY applications/datamanager/src/ applications/datamanager/src/

RUN cargo build --release --bin datamanager

FROM debian:bullseye-slim AS server 

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates openssl && \
    update-ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

ENV SSL_CERT_DIR=/etc/ssl/certs

WORKDIR /app

COPY --from=builder /app/target/release/datamanager /usr/local/bin

EXPOSE 8080

ENTRYPOINT ["/usr/local/bin/datamanager"]

