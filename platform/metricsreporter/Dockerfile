FROM lukemathwalker/cargo-chef:latest-rust-1.59.0 AS chef

RUN rustup toolchain install nightly

RUN rustup default nightly

WORKDIR app/

FROM chef AS planner

COPY libraries/rust/ libraries/rust/

COPY platform/metricsreporter/ platform/metricsreporter/

COPY Cargo.toml Cargo.toml

RUN echo '[workspace]\n\
resolver = "2"\n\
members = [\n\
    "libraries/rust",\n\
    "platform/metricsreporter",\n\ 
]\n\
' > Cargo.toml

COPY Cargo.lock Cargo.lock

RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder

COPY --from=planner /app/recipe.json recipe.json

RUN cargo chef cook --release --recipe-path recipe.json

COPY libraries/rust/src libraries/rust/src

COPY platform/metricsreporter/src platform/metricsreporter/src

RUN cargo build --release --bin metricsreporter

FROM debian:bullseye-slim AS runtime

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates openssl && \
    update-ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ENV SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

ENV SSL_CERT_DIR=/etc/ssl/certs

WORKDIR app

COPY --from=builder /app/target/release/metricsreporter /usr/local/bin

ENTRYPOINT ["/usr/local/bin/metricsreporter"]