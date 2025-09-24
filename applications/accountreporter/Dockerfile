# Use Rust base image for building
FROM rust:1.75 as builder

# Install dependencies required for Kafka client
RUN apt-get update && apt-get install -y \
    cmake \
    libclang-dev \
    libssl-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY Cargo.toml ./
COPY src ./src

# Build the application
RUN cargo build --release

# Use a smaller runtime image
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the binary from builder stage
COPY --from=builder /app/target/release/accountreporter /app/accountreporter

# Set environment variables with defaults
ENV ALPACA_BASE_URL=https://paper-api.alpaca.markets
ENV KAFKA_BROKERS=kafka:9092
ENV KAFKA_TOPIC=account-events
ENV POLL_INTERVAL_SECONDS=1
ENV RUST_LOG=accountreporter=info,rdkafka=warn

ENTRYPOINT ["./accountreporter"]