use crate::chunk;
use crate::chunk::Op;
use crate::error::Error;
use crate::value::Value;

macro_rules! pop_one {
    ($value_stack: ident, $op: expr) => {
        pop_one!($value_stack, $op, 1, 0)
    };
    ($value_stack: ident, $op: expr, $needed: expr, $actual: expr) => {
        match $value_stack.pop() {
            None => {
                return Err(Error::ValueStackTooSmall {
                    op: $op,
                    stack_size_needed: $needed,
                    stack_size_actual: $actual,
                })
            }
            Some(value) => value,
        }
    };
}

macro_rules! pop_two {
    ($value_stack: ident, $op: expr) => {{
        // It's a stack so the values are in reverse order
        let b = pop_one!($value_stack, $op, 2, 1);
        let a = pop_one!($value_stack, $op, 2, 0);
        (a, b)
    }};
}

pub fn run(chunk: &chunk::Chunk) -> Result<(), Error> {
    let mut ip: &[u8] = &chunk.bytecode;
    let mut value_stack = vec![];
    while !ip.is_empty() {
        let (op, new_ip) = Op::read(ip)?;
        match op {
            Op::Return => return Ok(()),
            Op::Constant(i) => {
                let value = match chunk.constants.get(i as usize) {
                    None => {
                        return Err(Error::InvalidConstantIndex {
                            op: op,
                            num_constants: chunk.constants.len(),
                        })
                    }
                    Some(value) => value,
                };
                value_stack.push(value.clone());
            }
            Op::Negate => match pop_one!(value_stack, Op::Negate) {
                Value::Number(d) => {
                    value_stack.push(Value::Number(-d));
                }
            },
            Op::Add => match pop_two!(value_stack, Op::Add) {
                (Value::Number(a), Value::Number(b)) => {
                    value_stack.push(Value::Number(a + b));
                }
            },
            Op::Subtract => match pop_two!(value_stack, Op::Subtract) {
                (Value::Number(a), Value::Number(b)) => {
                    value_stack.push(Value::Number(a - b));
                }
            },
            Op::Multiply => match pop_two!(value_stack, Op::Multiply) {
                (Value::Number(a), Value::Number(b)) => {
                    value_stack.push(Value::Number(a * b));
                }
            },
            Op::Divide => match pop_two!(value_stack, Op::Divide) {
                (Value::Number(a), Value::Number(b)) => {
                    value_stack.push(Value::Number(a / b));
                }
            },
        }
        println!("{:?}", op);
        for value in &value_stack {
            println!("[ {} ]", value);
        }
        ip = new_ip;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_subtractino() {}
}
