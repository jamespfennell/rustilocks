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
        serde::serialize_chunk(self)
    }

    pub fn deserialize(src: &[u8]) -> Result<Chunk, String> {
        serde::deserialize_chunk(src)
    }

    #[cfg(test)]
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

    pub fn serialize_to_assembly(&self) -> Result<String, Box<InvalidBytecodeError>> {
        let mut ip: &[u8] = &self.bytecode;
        let mut result = String::new();
        result.push_str("%%%% bytecode %%%%\n");
        while !ip.is_empty() {
            let (op_code, new_ip) = Op::read(ip)?;
            let new_line =
                op_code.write_to_assembly(self.bytecode.len() - ip.len(), &self.constants);
            result.push_str(&new_line);
            result.push('\n');
            ip = new_ip;
        }
        result.push_str("%%%% constants %%%%\n");
        for (i, value) in self.constants.iter().enumerate() {
            result.push_str(&format!("% {i}: {value}\n"))
        }
        Ok(result)
    }

    pub fn deserialize_from_assembly(source: &str) -> Chunk {
        let mut bytecode = vec![];
        let mut constants = vec![];
        for line in source.lines() {
            let op = match Op::read_from_assembly(line, &mut constants) {
                None => continue,
                Some(op) => op,
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
    Print,
    Pop,
    DefineGlobal(u8),
    GetGlobal(u8),
    SetGlobal(u8),
    GetLocal(u8),
    SetLocal(u8),
}

impl Op {
    /// Read reads the first op from the provided bytecode and returns the op
    /// and a pointer to code after the op.
    pub fn read(b: &[u8]) -> Result<(Op, &[u8]), Box<InvalidBytecodeError>> {
        let (op_code, mut tail) = match b.split_first() {
            None => return Err(Box::new(InvalidBytecodeError::EmptyBytecode)),
            Some((i, tail)) => (*i, tail),
        };
        let mut read_one = || match tail.split_first() {
            None => Err(Box::new(InvalidBytecodeError::MissingOpArgument {
                op: Op::Constant(0),
            })),
            Some((i, new_tail)) => {
                tail = new_tail;
                Ok(*i)
            }
        };
        let op = match op_code {
            0 => Op::Return,
            1 => Op::Constant(read_one()?),
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
            14 => Op::Print,
            15 => Op::Pop,
            16 => Op::DefineGlobal(read_one()?),
            17 => Op::GetGlobal(read_one()?),
            18 => Op::SetGlobal(read_one()?),
            19 => Op::GetLocal(read_one()?),
            20 => Op::SetLocal(read_one()?),
            _ => return Err(Box::new(InvalidBytecodeError::UnknownOpCode { op_code })),
        };
        Ok((op, tail))
    }

    pub fn write(&self, buffer: &mut Vec<u8>) {
        buffer.push(self.op_code());
        match self {
            Op::Constant(i)
            | Op::DefineGlobal(i)
            | Op::GetGlobal(i)
            | Op::SetGlobal(i)
            | Op::GetLocal(i)
            | Op::SetLocal(i) => {
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
            | Op::Less
            | Op::Print
            | Op::Pop => {}
        }
    }

    fn write_to_assembly(&self, offset: usize, constants: &[Value]) -> String {
        let command = match self {
            Op::Constant(_) => "CONSTANT",
            Op::Return => "RETURN",
            Op::Negate => "NEGATE",
            Op::Add => "ADD",
            Op::Subtract => "SUBTRACT",
            Op::Multiply => "MULTIPLY",
            Op::Divide => "DIVIDE",
            Op::True => "TRUE",
            Op::False => "FALSE",
            Op::Nil => "NIL",
            Op::Not => "NOT",
            Op::Equal => "EQUAL",
            Op::Greater => "GREATER",
            Op::Less => "LESS",
            Op::Print => "PRINT",
            Op::Pop => "POP",
            Op::DefineGlobal(_) => "DEFINE_GLOBAL",
            Op::GetGlobal(_) => "GET_GLOBAL",
            Op::SetGlobal(_) => "SET_GLOBAL",
            Op::GetLocal(_) => "GET_LOCAL",
            Op::SetLocal(_) => "SET_LOCAL",
        };
        let tail = match self {
            Op::Constant(i) | Op::DefineGlobal(i) | Op::GetGlobal(i) | Op::SetGlobal(i) => {
                let value = constants.get(*i as usize).unwrap();
                format!("{value} % constants_index={i}")
            }
            Op::GetLocal(i) | Op::SetLocal(i) => {
                format!("{i}")
            }
            _ => String::new(),
        };
        format!("{:04} {} {}", offset, command, tail)
    }

    fn read_from_assembly(line: &str, constants: &mut Vec<Value>) -> Option<Op> {
        let line = match line.find('%') {
            None => line,
            Some(i) => &line[..i],
        };
        let words: Vec<&str> = line.split_whitespace().collect();
        if words.is_empty() {
            return None;
        }
        let mut read_constant = || {
            let raw_value = words[2];
            match raw_value.chars().next().unwrap() {
                '0'..='9' => {
                    constants.push(Value::Number(raw_value.parse::<f64>().unwrap()));
                    (constants.len() - 1) as u8
                }
                _ => panic!("could not read constant"),
            }
        };
        let op = match words[1] {
            "RETURN" => Op::Return,
            "CONSTANT" => Op::Constant(read_constant()),
            "NEGATE" => Op::Negate,
            "ADD" => Op::Add,
            "SUBTRACT" => Op::Subtract,
            "MULTIPLY" => Op::Multiply,
            "DIVIDE" => Op::Divide,
            "TRUE" => Op::True,
            "FALSE" => Op::False,
            "NIL" => Op::Nil,
            "NOT" => Op::Not,
            "EQUAL" => Op::Equal,
            "GREATER" => Op::Greater,
            "LESS" => Op::Less,
            "PRINT" => Op::Print,
            "POP" => Op::Pop,
            "DEFINE_GLOBAL" => Op::DefineGlobal(read_constant()),
            "GET_GLOBAL" => Op::GetGlobal(read_constant()),
            "SET_GLOBAL" => Op::SetGlobal(read_constant()),
            "GET_LOCAL" => Op::GetLocal(read_constant()),
            "SET_LOCAL" => Op::SetLocal(read_constant()),
            _ => panic!("unknown command {}", words[1]),
        };
        Some(op)
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
            Op::Print => 14,
            Op::Pop => 15,
            Op::DefineGlobal(_) => 16,
            Op::GetGlobal(_) => 17,
            Op::SetGlobal(_) => 18,
            Op::GetLocal(_) => 19,
            Op::SetLocal(_) => 20,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! op_round_trip_test {
        ($(($name: ident, $op: expr),)+) => {
            $(
                mod $name {
                    use super::*;
                    #[test]
                    fn bytecode_round_trip() {
                        let op = $op;
                        let mut buffer = vec![];
                        op.write(&mut buffer);
                        let (out_op, tail) = Op::read(&buffer).unwrap();
                        assert_eq!(op, out_op);
                        assert!(tail.is_empty());
                    }

                    #[test]
                    fn assembly_round_trip() {
                        let op = $op;

                        let constants = vec![Value::Number(1.0), Value::Number(2.0)];
                        let assembly = op.write_to_assembly(0, &constants);
                        let mut constants = vec![Value::Number(1.0)];
                        let got_op = Op::read_from_assembly(&assembly, &mut constants).unwrap();

                        assert_eq!(got_op, op);
                    }
                }
            )+
        };
    }

    op_round_trip_test!(
        (constant, Op::Constant(1)),
        (return_, Op::Return),
        (negate, Op::Negate),
        (add, Op::Add),
        (subtract, Op::Subtract),
        (multiply, Op::Multiply),
        (divide, Op::Divide),
        (true_, Op::True),
        (false_, Op::False),
        (nil, Op::Nil),
        (not, Op::Not),
        (equal, Op::Equal),
        (greater, Op::Greater),
        (less, Op::Less),
        (print, Op::Print),
        (pop, Op::Pop),
        (define_global, Op::DefineGlobal(1)),
        (get_global, Op::GetGlobal(1)),
        (set_global, Op::SetGlobal(1)),
    );
}
