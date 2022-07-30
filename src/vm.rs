use crate::chunk;
use crate::chunk::Op;
use crate::error::{InvalidBytecodeError, RuntimeError};
use crate::value::{loxstring, Value};

pub fn run(chunk: &chunk::Chunk) -> Result<(), Box<RuntimeError>> {
    let mut ip: &[u8] = &chunk.bytecode;
    let mut value_stack = vec![];
    let mut string_interner: loxstring::Interner = Default::default();
    while !ip.is_empty() {
        let (op, new_ip) = Op::read(ip)?;
        match op {
            Op::Return => return Ok(()),
            Op::Constant(i) => {
                let constant_value = match chunk.constants.get(i as usize) {
                    None => {
                        return Err(Box::new(RuntimeError::InvalidBytecode(
                            InvalidBytecodeError::InvalidConstantIndex {
                                op: op,
                                num_constants: chunk.constants.len(),
                            },
                        )))
                    }
                    Some(value) => value,
                };
                let value = match constant_value {
                    Value::String(lox_string) => {
                        // We assume that strings can be compared by pointer value because they are interned
                        // in the same interner. The following logic copies the string from the chunk's interner (used
                        // only for interning constant strings) to the VM's interner. This way if the same string
                        // appears from different sources, it will be deduplicated correctly.
                        //
                        // As a concrete example, the source file may contain the string "hello world" and the
                        // the expression "hello" + " " + "world". The VM will resolve both of these to the
                        // same string, the first from reading the constant, and the second from performing
                        // string concatenation via OP_ADD. The result of both of these must end up in the same
                        // interner. Of course, the VM's interner is the only choice.
                        //
                        // Note this issue doesn't exist in the book because clox uses a global string interner
                        // inside the global VM instance.
                        let s = lox_string.as_str(&chunk.string_interner);
                        Value::String(string_interner.intern_ref(s))
                    }
                    _ => *constant_value,
                };
                value_stack.push(value);
            }
            // Unary operators
            Op::Negate | Op::Not => {
                let operand = pop_one(&mut value_stack, op)?;
                let result = match (op, operand) {
                    (Op::Not, _) => Value::Bool(operand.is_falsey()),
                    (Op::Negate, Value::Number(d)) => Value::Number(-d),
                    _ => {
                        return Err(Box::new(RuntimeError::InvalidTypeForUnaryOp {
                            operand,
                            op,
                        }))
                    }
                };
                value_stack.push(result);
            }
            // Binary operators
            Op::Add
            | Op::Subtract
            | Op::Multiply
            | Op::Divide
            | Op::Equal
            | Op::Greater
            | Op::Less => {
                let (left_operand, right_operand) = pop_two(&mut value_stack, op)?;
                let result = match (op, left_operand, right_operand) {
                    (Op::Equal, _, _) => Value::Bool(left_operand.equal(right_operand)),
                    (Op::Add, Value::String(a), Value::String(b)) => {
                        let a_ref = a.as_str(&string_interner);
                        let b_ref = b.as_str(&string_interner);
                        let mut c = String::with_capacity(a.len() + b.len());
                        c.push_str(a_ref);
                        c.push_str(b_ref);
                        Value::String(string_interner.intern_owned(c))
                    }
                    (Op::Add, Value::Number(a), Value::Number(b)) => Value::Number(a + b),
                    (Op::Subtract, Value::Number(a), Value::Number(b)) => Value::Number(a - b),
                    (Op::Multiply, Value::Number(a), Value::Number(b)) => Value::Number(a * b),
                    (Op::Divide, Value::Number(a), Value::Number(b)) => Value::Number(a / b),
                    (Op::Greater, Value::Number(a), Value::Number(b)) => Value::Bool(a > b),
                    (Op::Less, Value::Number(a), Value::Number(b)) => Value::Bool(a < b),
                    _ => {
                        return Err(Box::new(RuntimeError::InvalidTypeForBinaryOp {
                            left_operand,
                            right_operand,
                            op,
                        }))
                    }
                };
                value_stack.push(result);
            }
            // Literals
            Op::True => value_stack.push(Value::Bool(true)),
            Op::False => value_stack.push(Value::Bool(false)),
            Op::Nil => value_stack.push(Value::Nil),
        }
        println!("{:?}", op);
        for value in &value_stack {
            println!("[ {} ]", value);
        }
        ip = new_ip;
    }
    Ok(())
}

fn pop_one(value_stack: &mut Vec<Value>, op: Op) -> Result<Value, Box<InvalidBytecodeError>> {
    match value_stack.pop() {
        None => Err(Box::new(InvalidBytecodeError::ValueStackTooSmall {
            op,
            stack_size_needed: 1,
            stack_size_actual: 0,
        })),
        Some(value) => Ok(value),
    }
}

fn pop_two(
    value_stack: &mut Vec<Value>,
    op: Op,
) -> Result<(Value, Value), Box<InvalidBytecodeError>> {
    let rhs = match value_stack.pop() {
        None => Err(Box::new(InvalidBytecodeError::ValueStackTooSmall {
            op,
            stack_size_needed: 2,
            stack_size_actual: 0,
        })),
        Some(value) => Ok(value),
    }?;
    let lhs = match value_stack.pop() {
        None => Err(Box::new(InvalidBytecodeError::ValueStackTooSmall {
            op,
            stack_size_needed: 2,
            stack_size_actual: 1,
        })),
        Some(value) => Ok(value),
    }?;
    Ok((lhs, rhs))
}
