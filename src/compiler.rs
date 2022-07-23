use crate::chunk;
use crate::chunk::Op;
use crate::scanner::{self, Token, TokenType};
use crate::value::Value;

pub fn compile(src: &str) -> Result<chunk::Chunk, &str> {
    let mut compiler = Compiler {
        scanner: scanner::Scanner::new(src),
        chunk: chunk::Chunk {
            bytecode: vec![],
            constants: vec![],
        },
    };
    compiler.parse_precendence(Precedence::Assignment);
    compiler.emit_op(Op::Return);
    Ok(compiler.chunk)
}

struct Compiler<'a> {
    scanner: scanner::Scanner<'a>,
    chunk: chunk::Chunk,
}

impl<'a> Compiler<'a> {
    fn parse_precendence(&mut self, precedence: Precedence) {
        let token = match self.scanner.next().unwrap() {
            None => return,
            Some(token) => token,
        };
        let (prefix_rule, _, _) = get_rules(token.token_type);
        prefix_rule.unwrap()(self, token);

        loop {
            let token = match self.scanner.peek().unwrap() {
                None => return,
                Some(token) => token,
            };
            let (_, infix_rule, infix_precedence) = get_rules(token.token_type);

            if precedence > infix_precedence {
                break;
            }
            let infix_rule = match infix_rule {
                None => break,
                Some(infix_rule) => infix_rule,
            };
            self.scanner.consume().unwrap();
            infix_rule(self, token);
        }
    }

    fn emit_op(&mut self, op: Op) {
        op.write(&mut self.chunk.bytecode);
    }

    fn emit_constant(&mut self, constant: Value) -> u8 {
        let i = self.chunk.constants.len();
        self.chunk.constants.push(constant);
        match i.try_into() {
            Ok(i) => i,
            Err(_) => panic!("too many constants"),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Precedence {
    None = 0,
    Assignment = 1, // =
    Or = 2,         // or
    And = 3,        // and
    Equality = 4,   // == !=
    Comparison = 5, // < > <= >=
    Term = 6,       // + -
    Factor = 7,     // * /
    Unary = 8,      // ! -
    Call = 9,       // . ()
    Primary = 10,
}

type ParseRule = fn(s: &mut Compiler, Token);

fn get_rules(token_type: TokenType) -> (Option<ParseRule>, Option<ParseRule>, Precedence) {
    match token_type {
        TokenType::LeftParen => (Some(prefix_left_paren), None, Precedence::None),
        TokenType::RightParen => (None, None, Precedence::None),
        TokenType::Minus => (Some(prefix_minus), Some(infix_minus), Precedence::Term),
        TokenType::Plus => (None, Some(infix_plus), Precedence::Term),
        TokenType::Slash => (None, Some(infix_slash), Precedence::Factor),
        TokenType::Star => (None, Some(infix_star), Precedence::Factor),
        TokenType::Number => (Some(infix_number), None, Precedence::None),
        _ => panic!("unimplemented: {:?}", token_type),
    }
}

fn infix_minus(c: &mut Compiler, _: Token) {
    c.parse_precendence(Precedence::Factor);
    c.emit_op(Op::Subtract);
}

fn infix_number(s: &mut Compiler, token: Token) {
    let i = s.emit_constant(Value::Number(token.source.parse::<f64>().unwrap()));
    s.emit_op(Op::Constant(i));
}

fn infix_plus(c: &mut Compiler, _: Token) {
    c.parse_precendence(Precedence::Factor);
    c.emit_op(Op::Add);
}

fn infix_slash(c: &mut Compiler, _: Token) {
    c.parse_precendence(Precedence::Unary);
    c.emit_op(Op::Divide);
}

fn infix_star(c: &mut Compiler, _: Token) {
    c.parse_precendence(Precedence::Unary);
    c.emit_op(Op::Multiply);
}

fn prefix_left_paren(c: &mut Compiler, _: Token) {
    c.parse_precendence(Precedence::Assignment);
    if c.scanner.next().unwrap().unwrap().token_type != TokenType::RightParen {
        panic!("grouping not matched");
    }
}

fn prefix_minus(c: &mut Compiler, _: Token) {
    c.parse_precendence(Precedence::Unary);
    c.emit_op(Op::Negate);
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! compiler_test {
        ($name: ident, $input: expr, $want_ops: expr, $want_constants: expr) => {
            #[test]
            fn $name() {
                let mut want_ops = $want_ops;
                want_ops.push(Op::Return);

                let chunk = compile($input).unwrap();

                assert_eq!(chunk.constants, $want_constants);

                let mut got_ops = vec![];
                let mut bytecode: &[u8] = &chunk.bytecode;
                while !bytecode.is_empty() {
                    let (op, new_bytecode) = Op::read(&bytecode).unwrap();
                    got_ops.push(op);
                    bytecode = new_bytecode;
                }
                assert_eq!(got_ops, want_ops);
            }
        };
    }

    compiler_test!(
        prefix_minus,
        "-123",
        vec![Op::Constant(0), Op::Negate],
        vec![Value::Number(123.0)]
    );
    compiler_test!(
        infix_minus,
        "1-2",
        vec![Op::Constant(0), Op::Constant(1), Op::Subtract],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        infix_plus,
        "1+2",
        vec![Op::Constant(0), Op::Constant(1), Op::Add],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        infix_slash,
        "1/2",
        vec![Op::Constant(0), Op::Constant(1), Op::Divide],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        infix_star,
        "1*2",
        vec![Op::Constant(0), Op::Constant(1), Op::Multiply],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        addition_and_multiplication_1,
        "1*2+3",
        vec![
            Op::Constant(0),
            Op::Constant(1),
            Op::Multiply,
            Op::Constant(2),
            Op::Add
        ],
        vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]
    );
    compiler_test!(
        addition_and_multiplication_2,
        "1+2*3",
        vec![
            Op::Constant(0),
            Op::Constant(1),
            Op::Constant(2),
            Op::Multiply,
            Op::Add
        ],
        vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]
    );
    compiler_test!(
        negation_and_multiplication,
        "1*-2",
        vec![Op::Constant(0), Op::Constant(1), Op::Negate, Op::Multiply],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        grouping_1,
        "1*(2+3)",
        vec![Op::Constant(0), Op::Constant(1), Op::Constant(2), Op::Add, Op::Multiply],
        vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]
    );
}
