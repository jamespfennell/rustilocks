# Rustilocks

Work in progress implemention of the Lox bytecode intepreter in Robert Nystrom's book
    [Crafting Interpreters](https://craftinginterpreters.com/).

This is a Rust binary crate built with the clap CLI library, so `cargo run help`
    will print useful things.
The binary operates on 3 file formats:

- `.lox`: Lox source code.

- `.loxa`: Lox assembly. This is similar to the disassembly output in Crafting Interpreters
    except it can also be parsed back in and compiled to bytecode.

- `.rlks`: Lox binary code. This is a serialization of a compiled Lox program,
    and can be run directly by the Rustilocks interpreter.

## Goals

- Fully implement the interpreter to really understand how such interpreters work.

- Gain more experience with Rust.

## Non-goals

- Performance; prefer to use simple idiomatic Rust.

- Implementing hash maps, stacks, etc. by hand, as is done in the book; instead, use the Rust standard library.

## Status

- Chapter 14-23: done

- Chapter 24: next up.

- Chapter 24-26: to be started

- Chapter 27-29: I may not do these as OOP doesn't interest me that much.
    Maybe instead I'll add support for struct/dataclasses?
