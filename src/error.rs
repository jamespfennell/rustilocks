use crate::{chunk::Op, scanner::Token, value::Value};

#[derive(Debug)]
pub enum RuntimeError {
    InvalidBytecode(InvalidBytecodeError),
    InvalidTypeForUnaryOp {
        operand: Value,
        op: Op,
    },
    InvalidTypeForBinaryOp {
        left_operand: Value,
        right_operand: Value,
        op: Op,
    },
}

impl From<Box<InvalidBytecodeError>> for Box<RuntimeError> {
    fn from(e: Box<InvalidBytecodeError>) -> Self {
        Box::new(RuntimeError::InvalidBytecode(*e))
    }
}

#[derive(Debug)]
pub enum InvalidBytecodeError {
    ValueStackTooSmall {
        op: Op,
        stack_size_actual: usize,
        stack_size_needed: usize,
    },
    InvalidConstantIndex {
        op: Op,
        num_constants: usize,
    },
    UnknownOpCode {
        op_code: u8,
    },
    EmptyBytecode,
    MissingOpArgument {
        op: Op,
    },
}

#[derive(Debug)]
pub enum ScannerError<'a> {
    InvalidCharacter(&'a str),
    UnterminatedString(&'a str),
}

#[derive(Debug)]
pub enum CompilationError<'a> {
    ScannerError(ScannerError<'a>),
    TooManyConstants(Token<'a>),
    UnexpectedToken(Token<'a>, &'static str),
    MissingToken(&'static str),
    InvalidNumber(Token<'a>),
}

impl<'a> From<Box<ScannerError<'a>>> for Box<CompilationError<'a>> {
    fn from(e: Box<ScannerError<'a>>) -> Self {
        Box::new(CompilationError::ScannerError(*e))
    }
}
