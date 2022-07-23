# End-to-end tests

The [Crafting Interpreter's repository](https://github.com/munificent/craftinginterpreters)
    contains an end-to-end Lox test suite and facilities for
    running this suite over new implementations of the interpreter.
This directory contains the infrastructure to run the suite for Rustilocks.
To run the test suite, run the following command in the _root_ of the Rustilocks repository:

```
docker build -f tests/Dockerfile
```

If the Docker image builds succesfully, the tests pass.

The suite is designed to run over interpreters that are partially complete.
The chapter the interpreter has reached is specified in the Dart command in the Dockerfile.
This needs to be bumped as more chapters are finished.
