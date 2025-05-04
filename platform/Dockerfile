FROM haskell:9.12.2 AS builder

WORKDIR /app

RUN mkdir -p src app test && touch README.md

# Create minimum directory structure required by Stack
# this allows us to compile and cache dependencies
# between runs and then only need to compile our source
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack setup && stack build --only-dependencies

COPY . .

RUN stack build
RUN cp "$(stack path --local-install-root)/bin/platform-exe" /app/platform-exe

FROM debian:bullseye-slim AS production

RUN apt-get update && apt-get install -y libgmp10 ca-certificates && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/platform-exe /usr/local/bin/app

ENTRYPOINT ["/usr/local/bin/app"]
