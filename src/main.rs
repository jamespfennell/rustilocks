mod chunk;
mod value;

fn main() {
    let ops = vec![
        chunk::Op::Return,
        chunk::Op::Constant(0),
        chunk::Op::Constant(1),
    ];
    let mut bytecode = vec![];
    for op in &ops {
        op.write(&mut bytecode);
    }
    let chunk = chunk::Chunk {
        bytecode,
        constants: vec![value::Value::Number(0.1)],
    };
    chunk.disassemble().unwrap();
}
