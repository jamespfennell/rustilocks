use crate::{
    chunk::{Op, OpCode},
    scanner::Token,
};

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
        code: u8,
    },
    EmptyBytecode,
    MissingOpArgument {
        op_code: OpCode,
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
