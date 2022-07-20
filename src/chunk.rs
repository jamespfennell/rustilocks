use crate::value::Value;

pub struct Chunk {
    pub bytecode: Vec<u8>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn disassemble(&self) -> Result<(), &'static str> {
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
}

impl Op {
    /// Read reads the first op from the provided bytecode and returns the op
    /// and a pointer to code after the op.
    pub fn read(b: &[u8]) -> Result<(Op, &[u8]), &'static str> {
        let (i, tail) = match b.split_first() {
            None => return Err("bytecode is empty"),
            Some((i, tail)) => (*i, tail),
        };
        let op_code = match OpCode::try_from(i) {
            Err(()) => return Err("invalid op code"),
            Ok(op_code) => op_code,
        };
        match op_code {
            OpCode::Constant => {
                let (i, tail) = match tail.split_first() {
                    None => return Err("constant op must be followed by an index"),
                    Some((i, tail)) => (*i, tail),
                };
                Ok((Op::Constant(i), tail))
            }
            OpCode::Return => Ok((Op::Return, tail)),
        }
    }

    pub fn write(&self, buffer: &mut Vec<u8>) {
        buffer.push(self.op_code().into());
        match self {
            Op::Constant(i) => {
                buffer.push(*i);
            }
            Op::Return => {}
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
            Op::Return => {
                format!("RETURN")
            }
        };
        println!("{:04} {}", offset, text);
    }

    fn op_code(&self) -> OpCode {
        match self {
            Op::Constant(_) => OpCode::Constant,
            Op::Return => OpCode::Return,
        }
    }
}

/// OpCode is the code for a single instruction in Lox.
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum OpCode {
    Constant,
    Return,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(i: u8) -> Result<Self, Self::Error> {
        let op_code = match i {
            0 => OpCode::Return,
            1 => OpCode::Constant,
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_op_round_trip() {
        let all_ops = vec![Op::Constant(3), Op::Return];
        for op in all_ops {
            let mut buffer = vec![];
            op.write(&mut buffer);
            let (out_op, _) = Op::read(&buffer).unwrap();
            assert_eq!(op, out_op);
        }
    }

    #[test]
    fn test_op_code_round_trip() {
        let all_op_codes = vec![OpCode::Constant, OpCode::Return];
        for op_code in all_op_codes {
            let i: u8 = op_code.into();
            assert_eq!(op_code, i.try_into().unwrap())
        }
    }
}
