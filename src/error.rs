use std::fmt::Display;

use crate::{chunk::Op, scanner::Token, value::Value};

#[derive(Debug)]
pub struct RuntimeError {
    pub op: Op,
    pub line_number: Option<usize>,
    pub kind: RuntimeErrorKind,
}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    InvalidBytecode(InvalidBytecodeError),
    InvalidTypeForUnaryOp {
        operand: Value,
    },
    InvalidTypeForBinaryOp {
        left_operand: Value,
        right_operand: Value,
    },
    UndefinedVariable(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RuntimeErrorKind::*;
        match &self.kind {
            UndefinedVariable(variable_name) => {
                write!(f, "Undefined variable '{variable_name}'.")
            }
            InvalidTypeForUnaryOp { .. } => {
                write!(f, "Operand must be a number.")
            }
            InvalidTypeForBinaryOp { .. } => match self.op {
                Op::Add => write!(f, "Operands must be two numbers or two strings."),
                _ => write!(f, "Operands must be numbers."),
            },
            _ => write!(f, "todo {:?}", self.kind),
        }
    }
}

#[derive(Debug)]
pub enum InvalidBytecodeError {
    ValueStackTooSmall {
        stack_size_actual: u8,
        stack_size_needed: u8,
    },
    InvalidConstantIndex {
        index: u8,
        num_constants: usize,
    },
    UnknownOpCode {
        op_code: u8,
    },
    EmptyBytecode,
    MissingOpArgument {
        op_code: u8,
    },
    VariableNameNotString {
        value: Value,
    },
}

impl From<InvalidBytecodeError> for RuntimeErrorKind {
    fn from(value: InvalidBytecodeError) -> Self {
        RuntimeErrorKind::InvalidBytecode(value)
    }
}

#[derive(Debug)]
pub struct CompilationError {
    pub line_number: usize,
    pub at: String,
    pub kind: CompilationErrorKind,
}

#[derive(Debug)]
pub enum CompilationErrorKind {
    Todo(usize),
    //
    InvalidCharacter,
    UnterminatedString,
    TooManyConstants,
    ExpectedExpression,
    ExpectedIdentifier,
    InvalidNumber,
    InvalidAssignmentTarget,
    UnclosedBlock,
    TooManyLocals,
    LocalRedeclared,
    LocalUninitialized,
}

impl<'a> CompilationError {
    pub fn new(token: Token, kind: CompilationErrorKind) -> Box<Self> {
        Box::new(Self {
            line_number: token.line_number,
            at: token.source.into(),
            kind,
        })
    }
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn at(&'a self) -> Option<&'a str> {
        if self.at.is_empty() {
            None
        } else {
            Some(&self.at)
        }
    }
    pub fn message(&self) -> &'static str {
        use CompilationErrorKind::*;
        match self.kind {
            ExpectedExpression => "Expect expression.",
            InvalidAssignmentTarget => "Invalid assignment target.",
            UnterminatedString => "Unterminated string.",
            ExpectedIdentifier => "Expect variable name.",
            LocalRedeclared => "Already a variable with this name in this scope.",
            LocalUninitialized => "Can't read local variable in its own initializer.",
            _ => {
                println!("{:?}", self);
                "todo"
            }
        }
    }
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Error", self.line_number())?;
        if let Some(at) = self.at() {
            write!(f, " at '{}'", at)?;
        }
        write!(f, ": {}", self.message())
    }
}
