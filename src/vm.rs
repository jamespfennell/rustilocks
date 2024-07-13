use std::collections::HashMap;

use crate::chunk;
use crate::chunk::Op;
use crate::error::{InvalidBytecodeError, RuntimeError};
use crate::value::{loxstring, Value};

pub struct VM {
    print_fn: Box<dyn FnMut(Value)>,
    string_interner: loxstring::Interner,
    globals: HashMap<loxstring::LoxString, Value>,
}

impl Default for VM {
    fn default() -> Self {
        VM {
            print_fn: Box::new(|v| println!("{}", v)),
            string_interner: Default::default(),
            globals: Default::default(),
        }
    }
}

impl VM {
    pub fn run(&mut self, chunk: &chunk::Chunk) -> Result<(), Box<RuntimeError>> {
        let mut ip: &[u8] = &chunk.bytecode;
        let mut value_stack = vec![];
        while !ip.is_empty() {
            let (op, new_ip) = Op::read(ip)?;
            match op {
                Op::Return => return Ok(()),
                Op::Constant(i) => {
                    value_stack.push(self.read_constant(chunk, op, i)?);
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
                            let a_ref = a.as_str(&self.string_interner);
                            let b_ref = b.as_str(&self.string_interner);
                            let mut c = String::with_capacity(a.len() + b.len());
                            c.push_str(a_ref);
                            c.push_str(b_ref);
                            Value::String(self.string_interner.intern_owned(c))
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
                Op::Print => {
                    let value = pop_one(&mut value_stack, op)?;
                    (self.print_fn)(value);
                }
                Op::Pop => {
                    pop_one(&mut value_stack, op)?;
                }
                Op::DefineGlobal(i) => {
                    let name = self.read_constant_string(chunk, op, i)?;
                    let value = pop_one(&mut value_stack, op)?;
                    self.globals.insert(name, value);
                }
                Op::GetGlobal(i) => {
                    let name = self.read_constant_string(chunk, op, i)?;
                    let value = match self.globals.get(&name) {
                        None => {
                            return Err(Box::new(RuntimeError::UndefinedVariable(
                                name.as_str(&self.string_interner).into(),
                            )))
                        }
                        Some(value) => *value,
                    };
                    value_stack.push(value);
                }
                Op::SetGlobal(i) => {
                    let name = self.read_constant_string(chunk, op, i)?;
                    let new_value = pop_one(&mut value_stack, op)?;
                    match self.globals.get_mut(&name) {
                        None => {
                            return Err(Box::new(RuntimeError::UndefinedVariable(
                                name.as_str(&self.string_interner).into(),
                            )))
                        }
                        Some(value) => {
                            *value = new_value;
                        }
                    };
                    // This handles the Lox code `a = (b = 3);`. In general, assignment is an
                    // expression which leaves the value on the stack.
                    value_stack.push(new_value);
                }
            }
            ip = new_ip;
        }
        Ok(())
    }

    fn read_constant_string(
        &mut self,
        chunk: &chunk::Chunk,
        op: Op,
        i: u8,
    ) -> Result<loxstring::LoxString, Box<RuntimeError>> {
        let name = self.read_constant(chunk, op, i)?;
        match name {
            Value::String(s) => Ok(s),
            _ => Err(Box::new(RuntimeError::InvalidBytecode(
                InvalidBytecodeError::VariableNameNotString { op, value: name },
            ))),
        }
    }

    fn read_constant(
        &mut self,
        chunk: &chunk::Chunk,
        op: Op,
        i: u8,
    ) -> Result<Value, Box<RuntimeError>> {
        match chunk.constants.get(i as usize) {
            None => Err(Box::new(RuntimeError::InvalidBytecode(
                InvalidBytecodeError::InvalidConstantIndex {
                    op,
                    num_constants: chunk.constants.len(),
                },
            ))),
            Some(constant_value) => match constant_value {
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
                    Ok(Value::String(self.string_interner.intern_ref(s)))
                }
                _ => Ok(*constant_value),
            },
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::loxstring::Interner;
    use std::{cell::RefCell, rc::Rc};

    fn no_op_constants_preprocessor(_: &mut Interner, v: Vec<Value>) -> Vec<Value> {
        v
    }

    fn string_constants_preprocessor(string_interner: &mut Interner, v: Vec<&str>) -> Vec<Value> {
        let mut out = vec![];
        for s in v {
            out.push(Value::String(string_interner.intern_ref(s)));
        }
        out
    }

    macro_rules! vm_test {
        ($name: ident, $in_ops: expr, $in_constants: expr, $want: expr) => {
            vm_test!(
                $name,
                $in_ops,
                $in_constants,
                $want,
                no_op_constants_preprocessor
            );
        };
        ($name: ident, $in_ops: expr, $in_constants: expr, $want: expr, $constants_preprocessor: ident) => {
            #[test]
            fn $name() {
                let input = $in_ops;
                let mut bytecode = vec![];
                for op in input {
                    op.write(&mut bytecode);
                }
                let mut string_interner: Interner = Default::default();
                let constants = $constants_preprocessor(&mut string_interner, $in_constants);
                let chunk = chunk::Chunk {
                    bytecode,
                    constants,
                    string_interner,
                };

                // We use Rc of RefCell to avoid borrow checking pain
                let printed_values = Rc::new(RefCell::new(vec![]));
                let printed_values_2 = printed_values.clone();
                let mut string_interner: Interner = Default::default();
                let want = $constants_preprocessor(&mut string_interner, $want);
                let mut vm = VM {
                    print_fn: Box::new(move |v| {
                        printed_values_2.as_ref().borrow_mut().push(v);
                    }),
                    string_interner,
                    globals: Default::default(),
                };

                vm.run(&chunk).unwrap();
                let got = printed_values.take();

                assert_eq!(got, want);
            }
        };
    }

    macro_rules! binary_op_test {
        ($name: ident, $lhs: expr, $rhs: expr, $op: expr, $want: expr) => {
            vm_test!(
                $name,
                vec![Op::Constant(0), Op::Constant(1), $op, Op::Print],
                vec![$lhs, $rhs],
                vec![$want]
            );
        };
    }

    binary_op_test!(
        test_add_numbers,
        Value::Number(1.0),
        Value::Number(2.0),
        Op::Add,
        Value::Number(3.0)
    );
    binary_op_test!(
        test_subtract_numbers,
        Value::Number(1.0),
        Value::Number(2.0),
        Op::Subtract,
        Value::Number(-1.0)
    );
    binary_op_test!(
        test_multiply_numbers,
        Value::Number(3.0),
        Value::Number(2.0),
        Op::Multiply,
        Value::Number(6.0)
    );
    binary_op_test!(
        test_divide_numbers,
        Value::Number(7.0),
        Value::Number(2.0),
        Op::Divide,
        Value::Number(3.5)
    );
    binary_op_test!(
        test_greater_1,
        Value::Number(7.0),
        Value::Number(2.0),
        Op::Greater,
        Value::Bool(true)
    );
    binary_op_test!(
        test_greater_2,
        Value::Number(2.0),
        Value::Number(7.0),
        Op::Greater,
        Value::Bool(false)
    );
    binary_op_test!(
        test_less_1,
        Value::Number(7.0),
        Value::Number(2.0),
        Op::Less,
        Value::Bool(false)
    );
    binary_op_test!(
        test_less_2,
        Value::Number(2.0),
        Value::Number(7.0),
        Op::Less,
        Value::Bool(true)
    );
    binary_op_test!(
        test_equal_1,
        Value::Number(7.0),
        Value::Number(2.0),
        Op::Equal,
        Value::Bool(false)
    );
    binary_op_test!(
        test_equal_2,
        Value::Number(2.0),
        Value::Number(2.0),
        Op::Equal,
        Value::Bool(true)
    );
    vm_test!(
        test_add_strings,
        vec![Op::Constant(0), Op::Constant(1), Op::Add, Op::Print],
        vec!["hello", "world"],
        vec!["helloworld"],
        string_constants_preprocessor
    );
}
