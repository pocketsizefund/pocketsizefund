FROM rust:1.79.0 as build

RUN USER=root cargo new --bin dataprovider

WORKDIR /dataprovider

COPY ./Cargo.lock ./Cargo.lock

COPY ./Cargo.toml ./Cargo.toml

RUN cargo build --release

RUN rm src/*.rs

COPY ./src ./src

RUN rm ./target/release/deps/dataprovider*

RUN cargo build --release

FROM rust:1.79.0

COPY --from=build /dataprovider/target/release/dataprovider .

CMD ["./dataprovider"]
