use std::collections::HashMap;

use crate::error::InvalidBytecodeError;
use crate::serde;
use crate::value::loxstring;
use crate::value::Value;

#[derive(Debug)]
pub struct Chunk {
    pub bytecode: Vec<u8>,
    pub constants: Vec<Value>,
    pub string_interner: loxstring::Interner,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            bytecode: vec![],
            constants: vec![],
            string_interner: Default::default(),
        }
    }
    pub fn serialize(&self) -> Vec<u8> {
        serde::serialize_chunk(&self)
    }

    pub fn deserialize(src: &[u8]) -> Result<Chunk, String> {
        serde::deserialize_chunk(src)
    }

    pub fn convert_bytecode_to_ops(&self) -> Result<Vec<Op>, Box<InvalidBytecodeError>> {
        let mut ops = vec![];
        let mut bytecode: &[u8] = &self.bytecode;
        while !bytecode.is_empty() {
            let (op, new_bytecode) = Op::read(&bytecode)?;
            ops.push(op);
            bytecode = new_bytecode;
        }
        Ok(ops)
    }

    pub fn disassemble(&self) -> Result<(), Box<InvalidBytecodeError>> {
        println!("%%%% chunk %%%%");
        let mut ip: &[u8] = &self.bytecode;
        while !ip.is_empty() {
            let (op_code, new_ip) = Op::read(ip)?;
            op_code.disassemble(self.bytecode.len() - ip.len(), &self.constants);
            ip = new_ip;
        }
        Ok(())
    }

    pub fn assemble(source: &str) -> Chunk {
        let mut label_to_offset: HashMap<&str, usize> = HashMap::new();
        let mut bytecode = vec![];
        let mut constants = vec![];
        for line in source.lines() {
            let line = match line.find('%') {
                None => line,
                Some(i) => &line[..i],
            };
            let words: Vec<&str> = line.split_whitespace().collect();
            if words.is_empty() {
                continue;
            }
            label_to_offset.insert(words[0], bytecode.len());
            let op = match words[1] {
                "ADD" => Op::Add,
                "CONSTANT" => {
                    let raw_value = words[2];
                    match raw_value.chars().next().unwrap() {
                        '0'..='9' => {
                            constants.push(Value::Number(raw_value.parse::<f64>().unwrap()));
                            Op::Constant((constants.len() - 1) as u8)
                        }
                        _ => panic!("could not read constant"),
                    }
                }
                "MULTIPLY" => Op::Multiply,
                "NEGATE" => Op::Negate,
                "RETURN" => Op::Return,
                "SUBTRACT" => Op::Subtract,
                _ => panic!("unknown command {}", words[1]),
            };
            op.write(&mut bytecode);
        }
        Chunk {
            bytecode,
            constants,
            string_interner: Default::default(),
        }
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
    True,
    False,
    Nil,
    Not,
    Equal,
    Greater,
    Less,
}

impl Op {
    /// Read reads the first op from the provided bytecode and returns the op
    /// and a pointer to code after the op.
    pub fn read(b: &[u8]) -> Result<(Op, &[u8]), Box<InvalidBytecodeError>> {
        let (op_code, mut tail) = match b.split_first() {
            None => return Err(Box::new(InvalidBytecodeError::EmptyBytecode)),
            Some((i, tail)) => (*i, tail),
        };
        let op = match op_code {
            0 => Op::Return,
            1 => {
                let (i, new_tail) = match tail.split_first() {
                    None => {
                        return Err(Box::new(InvalidBytecodeError::MissingOpArgument {
                            op: Op::Constant(0),
                        }))
                    }
                    Some((i, tail)) => (*i, tail),
                };
                tail = new_tail;
                Op::Constant(i)
            }
            2 => Op::Negate,
            3 => Op::Add,
            4 => Op::Subtract,
            5 => Op::Multiply,
            6 => Op::Divide,
            7 => Op::True,
            8 => Op::False,
            9 => Op::Nil,
            10 => Op::Not,
            11 => Op::Equal,
            12 => Op::Greater,
            13 => Op::Less,
            _ => return Err(Box::new(InvalidBytecodeError::UnknownOpCode { op_code })),
        };
        Ok((op, tail))
    }

    pub fn write(&self, buffer: &mut Vec<u8>) {
        buffer.push(self.op_code());
        match self {
            Op::Constant(i) => {
                buffer.push(*i);
            }
            Op::Return
            | Op::Negate
            | Op::Add
            | Op::Subtract
            | Op::Multiply
            | Op::Divide
            | Op::True
            | Op::False
            | Op::Nil
            | Op::Not
            | Op::Equal
            | Op::Greater
            | Op::Less => {}
        }
    }

    fn disassemble(&self, offset: usize, constants: &[Value]) {
        let text = match self {
            Op::Constant(i) => {
                format!(
                    "CONSTANT {}",
                    match constants.get(*i as usize) {
                        None => format!("{} <invalid: only {} constants>", *i, constants.len()),
                        Some(value) => format!("{} %index={}", value, *i),
                    }
                )
            }
            Op::Return => format!("RETURN"),
            Op::Negate => format!("NEGATE"),
            Op::Add => format!("ADD"),
            Op::Subtract => format!("SUBTRACT"),
            Op::Multiply => format!("MULTIPLY"),
            Op::Divide => format!("DIVIDE"),
            Op::True => format!("TRUE"),
            Op::False => format!("FALSE"),
            Op::Nil => format!("NIL"),
            Op::Not => format!("NOT"),
            Op::Equal => format!("EQUAL"),
            Op::Greater => format!("GREATER"),
            Op::Less => format!("LESS"),
        };
        println!("{:04} {}", offset, text);
    }

    fn op_code(&self) -> u8 {
        match self {
            Op::Return => 0,
            Op::Constant(_) => 1,
            Op::Negate => 2,
            Op::Add => 3,
            Op::Subtract => 4,
            Op::Multiply => 5,
            Op::Divide => 6,
            Op::True => 7,
            Op::False => 8,
            Op::Nil => 9,
            Op::Not => 10,
            Op::Equal => 11,
            Op::Greater => 12,
            Op::Less => 13,
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
            Op::True,
            Op::False,
            Op::Nil,
            Op::Not,
            Op::Equal,
            Op::Greater,
            Op::Less,
        ];
        for op in all_ops {
            let mut buffer = vec![];
            op.write(&mut buffer);
            let (out_op, tail) = Op::read(&buffer).unwrap();
            assert_eq!(op, out_op);
            assert!(tail.is_empty());
        }
    }
}
