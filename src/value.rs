use std::fmt::Display;

#[derive(PartialEq, Clone, Copy, Debug, bincode::Decode, bincode::Encode)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(b) => !b,
            _ => false,
        }
    }

    pub fn equal(self, other: Value) -> bool {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(d) => d.fmt(f),
            Value::Bool(b) => b.fmt(f),
            Value::Nil => "<nil>".fmt(f),
        }
    }
}
