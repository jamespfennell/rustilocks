# Rustilocks

Rust implemention of the Lox bytecode intepreter in Robert Nystrom's book
    [Crafting Interpreters](https://craftinginterpreters.com/).

This is a Rust binary crate built with the clap CLI library, so `cargo run help`
    will print useful things.
The binary operates on 3 file formats:

- `.lox`: Lox source code.

- `.loxa`: Lox assembly. This is similar to the disassembly output in Crafting Interpreters
    except it can also be parsed back in and compiled to bytecode.

- `.rlks`: Lox binary code. ~~This is a serialization of a compiled Lox chunk.
    I currently use the bincode library for this, but may handroll my own (de)serialization code at some point.
    This is pretty easy as the bytecode proper is already binary - the non-trivial part
        is handling Lox values.~~ Not functioning for the moment while I figure out how to serialize/deserialize
        interned strings.

## Goals

- Fully implement the interpreter to really understand how such interpreters work.

- Gain more experience with Rust.

## Non-goals

- Performance; prefer to use simple idiomatic Rust.

- Implementing hash maps, stacks, etc. by hand, as is done in the book; instead, use the Rust standard library.

## Status

- Chapter 14-19: done

- Chapter 21: in progress.

- Chapter 22-29: to be started
