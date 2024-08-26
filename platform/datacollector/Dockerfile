FROM rust:1.79.0 as build

RUN USER=root cargo new --bin datacollector

WORKDIR /datacollector

COPY ./Cargo.lock ./Cargo.lock

COPY ./Cargo.toml ./Cargo.toml

RUN cargo build --release

RUN rm src/*.rs

COPY ./src ./src

RUN rm ./target/release/deps/datacollector*

RUN cargo build --release

FROM rust:1.79.0

COPY --from=build /datacollector/target/release/datacollector .

CMD ["./datacollector"]
