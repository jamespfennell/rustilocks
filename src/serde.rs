use crate::chunk;
use crate::value::loxstring;
use crate::value::loxstring::Interner;
use crate::value::Value;

pub fn serialize_chunk(chunk: &chunk::Chunk) -> Vec<u8> {
    let mut b = vec![];
    serialize_bytes(&mut b, &chunk.bytecode);
    let value_bytes = serialize_values(&chunk.constants, &chunk.string_interner);
    serialize_bytes(&mut b, &value_bytes);
    b
}

pub fn deserialize_chunk(b: &[u8]) -> Result<chunk::Chunk, String> {
    let (bytecode, tail) = deserialize_bytes(b)?;
    let (value_bytes, tail) = deserialize_bytes(tail)?;
    if !tail.is_empty() {
        return Err(format!("found {} trailing bytes", tail.len()));
    }
    let mut string_interner: Interner = Default::default();
    let constants = deserialize_values(&value_bytes, &mut string_interner)?;
    Ok(chunk::Chunk {
        bytecode: bytecode.into(),
        constants,
        string_interner,
    })
}

fn serialize_bytes(target: &mut Vec<u8>, b: &[u8]) {
    target.extend((b.len() as u64).to_be_bytes());
    target.extend(b);
}

fn deserialize_bytes(mut b: &[u8]) -> Result<(&[u8], &[u8]), String> {
    let (u_raw, tail) = match split_array::<8>(b) {
        None => return Err(format!("no 8 byte size value preceeding byte slice")),
        Some(t) => t,
    };
    b = tail;
    let len: usize = match u64::from_be_bytes(u_raw).try_into() {
        Ok(len) => len,
        Err(_) => return Err(format!("byte slice is to big for this architecture")),
    };
    let (head, tail) = match b.len() < len {
        true => return Err(format!("truncated byte slice")),
        false => b.split_at(len),
    };
    Ok((head, tail))
}

fn serialize_values(values: &[Value], interner: &loxstring::Interner) -> Vec<u8> {
    let mut b = vec![];
    for value in values {
        match *value {
            Value::Number(f) => {
                b.push(0);
                b.extend(f.to_be_bytes());
            }
            Value::Bool(bool) => {
                b.push(1);
                if bool {
                    b.push(0);
                } else {
                    b.push(1);
                }
            }
            Value::Nil => {
                b.push(2);
            }
            Value::String(l) => {
                b.push(3);
                let s = l.as_str(interner);
                serialize_bytes(&mut b, s.as_bytes());
            }
        }
    }
    b
}

fn deserialize_values(
    mut b: &[u8],
    interner: &mut loxstring::Interner,
) -> Result<Vec<Value>, String> {
    let mut values = vec![];
    while let Some(([code], tail)) = split_array::<1>(b) {
        b = tail;
        let value = match code {
            0 => {
                let (f_raw, tail) = match split_array::<8>(b) {
                    None => return Err(format!("no 8 byte value following number code")),
                    Some(t) => t,
                };
                b = tail;
                Value::Number(f64::from_be_bytes(f_raw))
            }
            1 => {
                let v = match b.get(0).copied() {
                    None => return Err(format!("no value following boolean code")),
                    Some(0) => true,
                    Some(1) => false,
                    Some(i) => return Err(format!("unknown value {} for boolean", i)),
                };
                b = &b[1..];
                Value::Bool(v)
            }
            2 => Value::Nil,
            3 => {
                let (s_raw, tail) = deserialize_bytes(b)?;
                b = tail;
                let s = match std::str::from_utf8(s_raw) {
                    Ok(s) => s,
                    Err(_) => return Err(format!("non-ut8 string")),
                };
                Value::String(interner.intern_ref(s))
            }
            _ => return Err(format!("unknown value code {}", code)),
        };
        values.push(value);
    }
    Ok(values)
}

fn split_array<const N: usize>(b: &[u8]) -> Option<([u8; N], &[u8])> {
    if b.len() < N {
        return None;
    }
    let (head, tail) = b.split_at(N);
    // Safety: from the split call, head is guaranteed to have 8 elements and the cast is infallible.
    let arr: [u8; N] = unsafe { head.try_into().unwrap_unchecked() };
    Some((arr, tail))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::loxstring::Interner;
    use crate::value::Value;

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

        let b = serialize_values(&values, &interner);

        let out = deserialize_values(&b, &mut interner).unwrap();

        assert_eq!(out, values);
    }
}
