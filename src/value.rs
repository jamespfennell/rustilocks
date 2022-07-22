use std::fmt::Display;

#[derive(Clone, bincode::Decode, bincode::Encode)]
pub enum Value {
    Number(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(d) => d.fmt(f),
        }
    }
}
