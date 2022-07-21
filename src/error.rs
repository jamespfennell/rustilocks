use crate::chunk::{Op, OpCode};

#[derive(Debug)]
pub enum Error {
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
