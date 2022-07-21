mod chunk;
mod error;
mod scanner;
mod value;
mod vm;

fn main() {
    let ops = vec![
        chunk::Op::Constant(0),
        chunk::Op::Negate,
        chunk::Op::Constant(0),
        chunk::Op::Multiply,
        chunk::Op::Return,
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
    vm::run(&chunk).unwrap();
}
