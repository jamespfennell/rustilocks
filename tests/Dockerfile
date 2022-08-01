FROM rust:1.62 AS rustilocks

WORKDIR /rustilocks
COPY Cargo.toml .
COPY Cargo.lock .
RUN mkdir src
RUN echo "fn main() {}" > src/main.rs
RUN cargo fetch
COPY src src
RUN cargo build --release

FROM dart:2.17

WORKDIR /rustilocks
COPY tests/craftinginterpreters/tool tool
COPY tests/craftinginterpreters/test test
RUN cd ./tool; dart pub get
COPY --from=rustilocks /rustilocks/target/release/rustilocks /bin/
RUN dart tool/bin/test.dart chap21_global -i rustilocks -a run
