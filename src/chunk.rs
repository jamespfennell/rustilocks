use crate::error::Error;
use crate::value::Value;

pub struct Chunk {
    pub bytecode: Vec<u8>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn disassemble(&self) -> Result<(), Error> {
        println!("%%%% chunk %%%%");
        let mut ip: &[u8] = &self.bytecode;
        while !ip.is_empty() {
            let (op_code, new_ip) = Op::read(ip)?;
            op_code.disassemble(self.bytecode.len() - ip.len(), &self.constants);
            ip = new_ip;
        }
        Ok(())
    }
}

/// Op is a single instruction in Lox.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Op {
    Constant(u8),
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Op {
    /// Read reads the first op from the provided bytecode and returns the op
    /// and a pointer to code after the op.
    pub fn read(b: &[u8]) -> Result<(Op, &[u8]), Error> {
        let (i, mut tail) = match b.split_first() {
            None => return Err(Error::EmptyBytecode),
            Some((i, tail)) => (*i, tail),
        };
        let op_code = match OpCode::try_from(i) {
            Err(()) => return Err(Error::UnknownOpCode { code: i }),
            Ok(op_code) => op_code,
        };
        let op = match op_code {
            OpCode::Constant => {
                let (i, new_tail) = match tail.split_first() {
                    None => return Err(Error::MissingOpArgument { op_code: op_code }),
                    Some((i, tail)) => (*i, tail),
                };
                tail = new_tail;
                Op::Constant(i)
            }
            OpCode::Return => Op::Return,
            OpCode::Negate => Op::Negate,
            OpCode::Add => Op::Add,
            OpCode::Subtract => Op::Subtract,
            OpCode::Multiply => Op::Multiply,
            OpCode::Divide => Op::Divide,
        };
        Ok((op, tail))
    }

    pub fn write(&self, buffer: &mut Vec<u8>) {
        buffer.push(self.op_code().into());
        match self {
            Op::Constant(i) => {
                buffer.push(*i);
            }
            Op::Return | Op::Negate | Op::Add | Op::Subtract | Op::Multiply | Op::Divide => {}
        }
    }

    fn disassemble(&self, offset: usize, constants: &[Value]) {
        let text = match self {
            Op::Constant(i) => {
                format!(
                    "CONSTANT {}",
                    match constants.get(*i as usize) {
                        None => format!("{} <invalid: only {} constants>", *i, constants.len()),
                        Some(value) => format!("{} '{}'", *i, value),
                    }
                )
            }
            Op::Return => format!("RETURN"),
            Op::Negate => format!("NEGATE"),
            Op::Add => format!("ADD"),
            Op::Subtract => format!("SUBTRACT"),
            Op::Multiply => format!("MULTIPLY"),
            Op::Divide => format!("DIVIDE"),
        };
        println!("{:04} {}", offset, text);
    }

    fn op_code(&self) -> OpCode {
        match self {
            Op::Constant(_) => OpCode::Constant,
            Op::Return => OpCode::Return,
            Op::Negate => OpCode::Negate,
            Op::Add => OpCode::Add,
            Op::Subtract => OpCode::Subtract,
            Op::Multiply => OpCode::Multiply,
            Op::Divide => OpCode::Divide,
        }
    }
}

/// OpCode is the code for a single instruction in Lox.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum OpCode {
    Constant,
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(i: u8) -> Result<Self, Self::Error> {
        let op_code = match i {
            0 => OpCode::Return,
            1 => OpCode::Constant,
            2 => OpCode::Negate,
            3 => OpCode::Add,
            4 => OpCode::Subtract,
            5 => OpCode::Multiply,
            6 => OpCode::Divide,
            _ => return Err(()),
        };
        Ok(op_code)
    }
}

impl From<OpCode> for u8 {
    fn from(op_code: OpCode) -> Self {
        match op_code {
            OpCode::Return => 0,
            OpCode::Constant => 1,
            OpCode::Negate => 2,
            OpCode::Add => 3,
            OpCode::Subtract => 4,
            OpCode::Multiply => 5,
            OpCode::Divide => 6,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_op_round_trip() {
        let all_ops = vec![
            Op::Constant(3),
            Op::Return,
            Op::Negate,
            Op::Add,
            Op::Subtract,
            Op::Multiply,
            Op::Divide,
        ];
        for op in all_ops {
            let mut buffer = vec![];
            op.write(&mut buffer);
            let (out_op, tail) = Op::read(&buffer).unwrap();
            assert_eq!(op, out_op);
            assert!(tail.is_empty());
        }
    }

    #[test]
    fn test_op_code_round_trip() {
        let all_op_codes = vec![
            OpCode::Constant,
            OpCode::Return,
            OpCode::Negate,
            OpCode::Add,
            OpCode::Subtract,
            OpCode::Multiply,
            OpCode::Divide,
        ];
        for op_code in all_op_codes {
            let i: u8 = op_code.into();
            assert_eq!(op_code, i.try_into().unwrap())
        }
    }
}
