use crate::chunk;
use crate::chunk::Op;
use crate::error::{InvalidBytecodeError, RuntimeError};
use crate::value::Value;

pub fn run(chunk: &chunk::Chunk) -> Result<(), Box<RuntimeError>> {
    let mut ip: &[u8] = &chunk.bytecode;
    let mut value_stack = vec![];
    while !ip.is_empty() {
        let (op, new_ip) = Op::read(ip)?;
        match op {
            Op::Return => return Ok(()),
            Op::Constant(i) => {
                let value = match chunk.constants.get(i as usize) {
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
                value_stack.push(value.clone());
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
