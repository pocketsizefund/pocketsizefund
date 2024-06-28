ARG RUST_VERSION=1.79.0
ARG APP_NAME=newsfeed

FROM rust:${RUST_VERSION}-alpine AS build
ARG APP_NAME
WORKDIR /app

RUN apk add --no-cache \
    clang \
    lld \
    musl-dev \
    git \
    build-base \
    perl \
    openssl-dev \
    pkgconfig

ENV OPENSSL_DIR=/usr/include/openssl
ENV OPENSSL_STATIC=1

RUN --mount=type=bind,source=src,target=src \
    --mount=type=bind,source=Cargo.toml,target=Cargo.toml \
    --mount=type=bind,source=Cargo.lock,target=Cargo.lock \
    --mount=type=cache,target=/app/target/ \
    --mount=type=cache,target=/usr/local/cargo/git/db \
    --mount=type=cache,target=/usr/local/cargo/registry/ \
    cargo build --locked --release && \
    cp ./target/release/$APP_NAME /bin/server

FROM alpine:3.18 AS final

RUN apk add --no-cache libgcc

ARG UID=10001
RUN adduser \
    --disabled-password \
    --gecos "" \
    --home "/nonexistent" \
    --shell "/sbin/nologin" \
    --no-create-home \
    --uid "${UID}" \
    appuser
USER appuser

COPY --from=build /bin/server /bin/

EXPOSE 8080

CMD ["/bin/server"]
