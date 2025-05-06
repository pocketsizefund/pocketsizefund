FROM haskell:9.8.4 AS builder

WORKDIR /app

# -----------------

# Install wget, unzip
RUN apt-get update && apt-get install -y wget unzip && rm -rf /var/lib/apt/lists/*

# Install DuckDB 1.2.2 (latest stable)[](https://pypi.org/project/duckdb/)
# RUN wget https://github.com/duckdb/duckdb/releases/download/v1.2.2/libduckdb-linux-amd64.zip \
#     && unzip libduckdb-linux-amd64.zip -d /usr/lib \
RUN wget https://github.com/duckdb/duckdb/releases/download/v1.2.2/libduckdb-linux-aarch64.zip \
    && unzip libduckdb-linux-aarch64.zip -d /usr/lib \
    && cp /usr/lib/duckdb.h /usr/include/duckdb.h \
    && ldconfig

# -----------------

# RUN apt-get update && apt-get install -y unzip && rm -rf /var/lib/apt/lists/*

# RUN curl https://github.com/duckdb/duckdb/releases/download/v1.2.2/duckdb_cli-linux-aarch64.zip

# RUN unzip duckdb_cli-linux-aarch64.zip

# -----------------

COPY stack.yaml stack.yaml.lock ./

RUN mkdir -p platform/datamanager/{src,app}

COPY platform/datamanager/package.yaml platform/datamanager/package.yaml

RUN mkdir -p platform/shared

COPY platform/shared/package.yaml platform/shared/package.yaml

RUN stack setup

RUN stack build --only-dependencies

COPY platform/datamanager/ platform/datamanager/

COPY platform/shared/ platform/shared/

RUN stack build datamanager

RUN cp "$(stack path --local-install-root)/bin/datamanager" /app/datamanager

# --- TEMP ---

FROM debian:bullseye-slim AS runner

# -----------------

# RUN apt-get update && apt-get install -y libgmp10 ca-certificates libduckdb && rm -rf /var/lib/apt/lists/*

# -----------------

# Install runtime dependencies
RUN apt-get update && apt-get install -y libgmp10 ca-certificates wget unzip && rm -rf /var/lib/apt/lists/*

# Install DuckDB 1.2.2
# RUN wget https://github.com/duckdb/duckdb/releases/download/v1.2.2/libduckdb-linux-amd64.zip \
#     && unzip libduckdb-linux-amd64.zip -d /usr/lib \
RUN wget https://github.com/duckdb/duckdb/releases/download/v1.2.2/libduckdb-linux-aarch64.zip \
    && unzip libduckdb-linux-aarch64.zip -d /usr/lib \
    && ldconfig

# -----------------

COPY --from=builder /app/datamanager /usr/local/bin/app

ENTRYPOINT ["/usr/local/bin/app"]
