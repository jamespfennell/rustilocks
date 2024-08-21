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

## E2E diff testing

The [Crafting Interpreter's repository](https://github.com/munificent/craftinginterpreters)
    contains an awesome end-to-end Lox test suite and facilities for
    running this suite over new implementations of the interpreter, like Rustilocks.
However our experience is that this test suite is not 100% exhaustive,
    especially on error edge cases.
Adding new test cases to the test suite is a little tedious as one has to manually
    annotate the Lox source files with the expected results,
    and wire up the cases into the Dart test runner.

As an alternative, this project uses diff testing,
    in which the output of the Rustilocks interpreter and clox is compared
    over arbitrary Lox source files.
Adding a new test case just means adding an arbitrary new Lox file.

To run the diff tests, clox has to be built first:

```
cd tests/craftinginterpreters && make clox && cd -
```

The diff tests are then run using the usual `cargo t` command.

The test cases come from two sources:

- The E2E test suite in the Crafting Interpreters repository.
  The Lox files are at `tests/craftinginterpreters/test` in this repo.

- Additional new Rustilocks test cases under `tests`.
