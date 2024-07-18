use crate::chunk;
use crate::value::loxstring;
use crate::value::loxstring::Interner;
use crate::value::Value;

pub fn serialize_chunk(chunk: &chunk::Chunk) -> Vec<u8> {
    let mut buffer = vec![];
    chunk
        .bytecode
        .serialize(&mut buffer, &chunk.string_interner);
    chunk
        .constants
        .serialize(&mut buffer, &chunk.string_interner);
    buffer
}

pub fn deserialize_chunk(b: &[u8]) -> Result<chunk::Chunk, String> {
    let mut buffer = Buffer { b };
    let mut string_interner: Interner = Default::default();
    let bytecode = Vec::<u8>::deserialize(&mut buffer, &mut string_interner)?;
    let constants = Vec::<Value>::deserialize(&mut buffer, &mut string_interner)?;
    if !buffer.b.is_empty() {
        return Err(format!("found {} trailing bytes", buffer.b.len()));
    }
    Ok(chunk::Chunk {
        bytecode,
        constants,
        string_interner,
    })
}

struct Buffer<'a> {
    b: &'a [u8],
}
impl<'a> Buffer<'a> {
    fn take<const N: usize>(&mut self, type_name: &'static str) -> Result<[u8; N], String> {
        if self.b.len() < N {
            return Err(format!("expected a {N} byte value for {type_name}"));
        }
        let (head, tail) = self.b.split_at(N);
        // Safety: from the split call, head is guaranteed to have 8 elements and the cast is infallible.
        let head: [u8; N] = unsafe { head.try_into().unwrap_unchecked() };
        self.b = tail;
        Ok(head)
    }
}

trait Serde: Sized {
    fn serialize(&self, buffer: &mut Vec<u8>, interner: &loxstring::Interner);
    fn deserialize(buffer: &mut Buffer, interner: &mut loxstring::Interner)
        -> Result<Self, String>;
}

macro_rules! numeric_serde_impl {
    ( $( ($type_name: ident, $type_name_str: expr, $num_bytes: expr ), )+ ) => { $(
        impl Serde for $type_name {
            fn serialize(&self, buffer: &mut Vec<u8>, _: &loxstring::Interner) {
                buffer.extend(self.to_be_bytes());
            }
            fn deserialize(buffer: &mut Buffer, _: &mut loxstring::Interner) -> Result<Self, String> {
                Ok($type_name::from_be_bytes(buffer.take::<$num_bytes>($type_name_str)?))
            }
        }
    )+ };
}

numeric_serde_impl!((f64, "f64", 8), (u64, "u64", 8), (u8, "u8", 1),);

impl Serde for bool {
    fn serialize(&self, buffer: &mut Vec<u8>, _: &loxstring::Interner) {
        buffer.push(if *self { 0 } else { 1 })
    }
    fn deserialize(buffer: &mut Buffer, _: &mut loxstring::Interner) -> Result<Self, String> {
        let [b] = buffer.take::<1>("bool")?;
        match b {
            0 => Ok(true),
            1 => Ok(false),
            _ => Err(format!("invalid value {b} for bool")),
        }
    }
}

impl<T: Serde> Serde for &[T] {
    fn serialize(&self, buffer: &mut Vec<u8>, interner: &loxstring::Interner) {
        (self.len() as u64).serialize(buffer, interner);
        for elem in self.iter() {
            elem.serialize(buffer, interner);
        }
    }
    fn deserialize(_: &mut Buffer, _: &mut loxstring::Interner) -> Result<Self, String> {
        unimplemented!()
    }
}

impl<T: Serde> Serde for Vec<T> {
    fn serialize(&self, buffer: &mut Vec<u8>, interner: &loxstring::Interner) {
        self.as_slice().serialize(buffer, interner);
    }
    fn deserialize(
        buffer: &mut Buffer,
        interner: &mut loxstring::Interner,
    ) -> Result<Self, String> {
        let len = u64::deserialize(buffer, interner)? as usize;
        let mut v = Vec::with_capacity(len);
        for _ in 0..len {
            v.push(T::deserialize(buffer, interner)?);
        }
        Ok(v)
    }
}

impl Serde for loxstring::LoxString {
    fn serialize(&self, buffer: &mut Vec<u8>, interner: &loxstring::Interner) {
        self.as_str(interner).as_bytes().serialize(buffer, interner);
    }
    fn deserialize(
        buffer: &mut Buffer,
        interner: &mut loxstring::Interner,
    ) -> Result<Self, String> {
        let bytes = Vec::<u8>::deserialize(buffer, interner)?;
        let s = match String::from_utf8(bytes) {
            Ok(v) => v,
            Err(e) => return Err(format!("invalid UTF-8 sequence: {}", e)),
        };
        Ok(interner.intern_owned(s))
    }
}

impl Serde for Value {
    fn serialize(&self, buffer: &mut Vec<u8>, interner: &loxstring::Interner) {
        match self {
            Value::Number(f) => {
                buffer.push(0);
                f.serialize(buffer, interner);
            }
            Value::Bool(b) => {
                buffer.push(1);
                b.serialize(buffer, interner);
            }
            Value::Nil => {
                buffer.push(2);
            }
            Value::String(s) => {
                buffer.push(3);
                s.serialize(buffer, interner);
            }
        }
    }
    fn deserialize(
        buffer: &mut Buffer,
        interner: &mut loxstring::Interner,
    ) -> Result<Self, String> {
        let [d] = buffer.take::<1>("Value")?;
        match d {
            0 => Ok(Value::Number(f64::deserialize(buffer, interner)?)),
            1 => Ok(Value::Bool(bool::deserialize(buffer, interner)?)),
            2 => Ok(Value::Nil),
            3 => Ok(Value::String(loxstring::LoxString::deserialize(
                buffer, interner,
            )?)),
            _ => Err(format!("invalid discriminant {d} for Value")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn serde_chunk() {
        let chunk = chunk::Chunk {
            bytecode: vec![1, 201, 3, 4],
            constants: vec![Value::Number(3.14)],
            string_interner: Default::default(),
        };

        let b = serialize_chunk(&chunk);
        let out_chunk = deserialize_chunk(&b).unwrap();

        assert_eq!(out_chunk.bytecode, chunk.bytecode);
        assert_eq!(out_chunk.constants, chunk.constants);
    }

    #[test]
    fn serde_values() {
        let mut interner: Interner = Default::default();
        let s_1 = interner.intern_ref("hello");
        let s_2 = interner.intern_ref("world");
        let s_3 = interner.intern_ref("");
        let values = vec![
            Value::String(s_1),
            Value::Number(1.34),
            Value::Bool(true),
            Value::String(s_2),
            Value::Bool(false),
            Value::String(s_3),
            Value::Nil,
        ];

        let mut b = vec![];
        values.serialize(&mut b, &interner);
        let mut buffer = Buffer { b: &b };
        let out = Vec::<Value>::deserialize(&mut buffer, &mut interner).unwrap();

        assert_eq!(out, values);
    }
}
