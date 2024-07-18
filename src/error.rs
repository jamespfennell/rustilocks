use std::fmt::Display;

use crate::{
    chunk::Op,
    scanner::{Token, TokenType},
    value::Value,
};

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
    UndefinedVariable(String),
}

impl From<Box<InvalidBytecodeError>> for Box<RuntimeError> {
    fn from(e: Box<InvalidBytecodeError>) -> Self {
        Box::new(RuntimeError::InvalidBytecode(*e))
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      use RuntimeError::*;
      match self {
        UndefinedVariable(variable_name) => {
          write!(f, "Undefined variable '{variable_name}'.")
        }
        _ => write!(f, "todo {:?}", self)
      }
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
    VariableNameNotString {
        op: Op,
        value: Value,
    },
}

#[derive(Debug)]
pub enum ScannerError<'a> {
    InvalidCharacter(&'a str),
    UnterminatedString(usize, &'a str),
}

#[derive(Debug)]
pub enum CompilationError<'a> {
    Scanner(ScannerError<'a>),
    TooManyConstants(Token<'a>),
    ExpectedExpression(Token<'a>),
    UnexpectedToken(Token<'a>, &'static str),
    UnexpectedTokenType(Option<Token<'a>>, TokenType),
    MissingToken(&'static str),
    InvalidNumber(Token<'a>),
    InvalidAssignmentTarget(Token<'a>),
    UnclosedBlock(Token<'a>),
    TooManyLocals(Token<'a>),
    LocalRedeclared(Token<'a>),
}

impl<'a> CompilationError<'a> {
    pub fn line_number(&self) -> usize {
        use CompilationError::*;
        match self {
            ExpectedExpression(token) | InvalidAssignmentTarget(token) => token.line_number,
            _ => 0,
        }
    }

    pub fn at(&self) -> Option<&'a str> {
        use CompilationError::*;
        match self {
            ExpectedExpression(token) | InvalidAssignmentTarget(token) => Some(token.source),
            _ => None,
        }
    }
    pub fn message(&self) -> &'static str {
        use CompilationError::*;
        use ScannerError::*;
        match self {
            ExpectedExpression(_) => "Expect expression.",
            InvalidAssignmentTarget(_) => "Invalid assignment target.",
            Scanner(UnterminatedString(..)) => "Unterminated string.",
            _ => {
                println!("{:?}", self);
                "todo"
            }
        }
    }
}

impl<'a> Display for CompilationError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Error", self.line_number())?;
        if let Some(at) = self.at() {
            write!(f, " at '{}'", at)?;
        }
        write!(f, ": {}", self.message())
    }
}

impl<'a> From<Box<ScannerError<'a>>> for Box<CompilationError<'a>> {
    fn from(e: Box<ScannerError<'a>>) -> Self {
        Box::new(CompilationError::Scanner(*e))
    }
}
