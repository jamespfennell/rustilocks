FROM dart:2 as testrunner

WORKDIR /rustilocks
COPY tests/craftinginterpreters/tool tool
RUN cd ./tool; dart pub get
RUN dart compile exe tool/bin/test.dart -o testrunner


FROM rust:1.80 AS rustilocks

WORKDIR /rustilocks
COPY tests/craftinginterpreters/test test
COPY --from=testrunner /rustilocks/testrunner /bin

COPY Cargo.toml .
COPY Cargo.lock .
RUN mkdir src
RUN echo 'fn main() { println!["dummy binary"]; }' > src/main.rs
RUN cargo fetch
RUN cargo build --release
COPY src src
# We need to advance the timestamp of main.rs so the compiler knows it's been updated
RUN touch src/main.rs
RUN cargo build --release
RUN cp /rustilocks/target/release/rustilocks /bin

# For future chapters, the E2E test ID can be obtained here:
# https://github.com/munificent/craftinginterpreters/blob/01e6f5b8f3e5dfa65674c2f9cf4700d73ab41cf8/tool/bin/test.dart#L754-L818
RUN testrunner chap23_jumping -i rustilocks -a run

