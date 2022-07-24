use crate::chunk;
use crate::chunk::Op;
use crate::error::CompilationError;
use crate::scanner::{self, Token, TokenType};
use crate::value::Value;

pub fn compile(src: &str) -> Result<chunk::Chunk, Box<CompilationError>> {
    let mut compiler = Compiler {
        scanner: scanner::Scanner::new(src),
        chunk: chunk::Chunk {
            bytecode: vec![],
            constants: vec![],
        },
    };
    compiler.expression()?;
    compiler.emit_op(Op::Return);
    Ok(compiler.chunk)
}

struct Compiler<'a> {
    scanner: scanner::Scanner<'a>,
    chunk: chunk::Chunk,
}

impl<'a> Compiler<'a> {
    fn expression(&mut self) -> Result<(), Box<CompilationError<'a>>> {
        self.parse_precendence(Precedence::Assignment)
    }

    fn parse_precendence(
        &mut self,
        precedence: Precedence,
    ) -> Result<(), Box<CompilationError<'a>>> {
        let token = match self.scanner.next()? {
            None => return Ok(()),
            Some(token) => token,
        };
        let (prefix_rule, _, _) = get_rules(token.token_type);
        let prefix_rule = match prefix_rule {
            None => {
                return Err(Box::new(CompilationError::UnexpectedToken(
                    token,
                    "expected a prefix token",
                )))
            }
            Some(prefix_rule) => prefix_rule,
        };
        prefix_rule(self, token)?;

        loop {
            let token = match self.scanner.peek()? {
                None => break,
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
            self.scanner.consume()?;
            infix_rule(self, token)?;
        }
        Ok(())
    }

    fn emit_op(&mut self, op: Op) {
        op.write(&mut self.chunk.bytecode);
    }

    fn emit_constant(
        &mut self,
        token: Token<'a>,
        constant: Value,
    ) -> Result<u8, Box<CompilationError<'a>>> {
        let i = self.chunk.constants.len();
        self.chunk.constants.push(constant);
        match i.try_into() {
            Ok(i) => Ok(i),
            Err(_) => Err(Box::new(CompilationError::TooManyConstants(token))),
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

type ParseRule<'a> = fn(s: &mut Compiler<'a>, Token<'a>) -> Result<(), Box<CompilationError<'a>>>;

fn get_rules<'a>(
    token_type: TokenType,
) -> (Option<ParseRule<'a>>, Option<ParseRule<'a>>, Precedence) {
    match token_type {
        TokenType::LeftParen => (Some(prefix_left_paren), None, Precedence::None),
        TokenType::RightParen => (None, None, Precedence::None),
        TokenType::Minus => (Some(prefix_minus), Some(infix_binary), Precedence::Term),
        TokenType::Plus => (None, Some(infix_binary), Precedence::Term),
        TokenType::Slash => (None, Some(infix_binary), Precedence::Factor),
        TokenType::Star => (None, Some(infix_binary), Precedence::Factor),
        TokenType::Bang => (Some(prefix_bang), None, Precedence::Unary),
        TokenType::BangEqual => (None, Some(infix_binary), Precedence::Equality),
        TokenType::EqualEqual => (None, Some(infix_binary), Precedence::Equality),
        TokenType::Greater => (None, Some(infix_binary), Precedence::Comparison),
        TokenType::GreaterEqual => (None, Some(infix_binary), Precedence::Comparison),
        TokenType::Less => (None, Some(infix_binary), Precedence::Comparison),
        TokenType::LessEqual => (None, Some(infix_binary), Precedence::Comparison),
        TokenType::Number => (Some(prefix_number), None, Precedence::None),
        TokenType::False => (Some(prefix_false), None, Precedence::None),
        TokenType::Nil => (Some(prefix_nil), None, Precedence::None),
        TokenType::True => (Some(prefix_true), None, Precedence::None),
        _ => panic!("unimplemented compilation rule for token: {:?}", token_type),
    }
}

// TODO: if this turns out to be the only infix function then we should refactor the code to
// not go through the rules at all. This way we could avoid double matching on the token type.
fn infix_binary<'a>(c: &mut Compiler<'a>, token: Token) -> Result<(), Box<CompilationError<'a>>> {
    let (next_precedence, op_1, op_2) = match token.token_type {
        TokenType::Plus => (Precedence::Factor, Op::Add, None),
        TokenType::Minus => (Precedence::Factor, Op::Subtract, None),
        TokenType::Star => (Precedence::Unary, Op::Multiply, None),
        TokenType::Slash => (Precedence::Unary, Op::Divide, None),
        TokenType::BangEqual => (Precedence::Comparison, Op::Equal, Some(Op::Not)),
        TokenType::EqualEqual => (Precedence::Comparison, Op::Equal, None),
        TokenType::Greater => (Precedence::Term, Op::Greater, None),
        TokenType::GreaterEqual => (Precedence::Term, Op::Less, Some(Op::Not)),
        TokenType::Less => (Precedence::Term, Op::Less, None),
        TokenType::LessEqual => (Precedence::Term, Op::Greater, Some(Op::Not)),
        _ => unreachable!(),
    };
    c.parse_precendence(next_precedence)?;
    c.emit_op(op_1);
    if let Some(op_2) = op_2 {
        c.emit_op(op_2);
    }
    Ok(())
}

fn prefix_bang<'a>(c: &mut Compiler<'a>, _: Token) -> Result<(), Box<CompilationError<'a>>> {
    c.expression()?;
    c.emit_op(Op::Not);
    Ok(())
}

fn prefix_false<'a>(c: &mut Compiler<'a>, _: Token) -> Result<(), Box<CompilationError<'a>>> {
    c.emit_op(Op::False);
    Ok(())
}

fn prefix_left_paren<'a>(c: &mut Compiler<'a>, _: Token) -> Result<(), Box<CompilationError<'a>>> {
    c.expression()?;
    match c.scanner.next()? {
        None => Err(Box::new(CompilationError::MissingToken(
            "expected a closing parenthesis",
        ))),
        Some(t) => match t.token_type {
            TokenType::RightParen => Ok(()),
            _ => Err(Box::new(CompilationError::UnexpectedToken(
                t,
                "expected a closing parenthesis",
            ))),
        },
    }
}

fn prefix_minus<'a>(c: &mut Compiler<'a>, _: Token) -> Result<(), Box<CompilationError<'a>>> {
    c.expression()?;
    c.emit_op(Op::Negate);
    Ok(())
}

fn prefix_nil<'a>(c: &mut Compiler<'a>, _: Token) -> Result<(), Box<CompilationError<'a>>> {
    c.emit_op(Op::Nil);
    Ok(())
}

fn prefix_number<'a>(
    s: &mut Compiler<'a>,
    token: Token<'a>,
) -> Result<(), Box<CompilationError<'a>>> {
    let d = match token.source.parse::<f64>() {
        Ok(d) => d,
        Err(_) => return Err(Box::new(CompilationError::InvalidNumber(token))),
    };
    let i = s.emit_constant(token, Value::Number(d))?;
    s.emit_op(Op::Constant(i));
    Ok(())
}

fn prefix_true<'a>(c: &mut Compiler<'a>, _: Token) -> Result<(), Box<CompilationError<'a>>> {
    c.emit_op(Op::True);
    Ok(())
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
        vec![
            Op::Constant(0),
            Op::Constant(1),
            Op::Constant(2),
            Op::Add,
            Op::Multiply
        ],
        vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]
    );
    compiler_test!(prefix_true, "true", vec![Op::True], vec![]);
    compiler_test!(prefix_false, "false", vec![Op::False], vec![]);
    compiler_test!(prefix_nil, "nil", vec![Op::Nil], vec![]);
    compiler_test!(prefix_not, "!", vec![Op::Not], vec![]);
    compiler_test!(boolean, "!true", vec![Op::True, Op::Not], vec![]);
    compiler_test!(
        prefix_bang_equal,
        "1!=2",
        vec![Op::Constant(0), Op::Constant(1), Op::Equal, Op::Not],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_equal_equal,
        "1==2",
        vec![Op::Constant(0), Op::Constant(1), Op::Equal],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_less_equal,
        "1<=2",
        vec![Op::Constant(0), Op::Constant(1), Op::Greater, Op::Not],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_less,
        "1<2",
        vec![Op::Constant(0), Op::Constant(1), Op::Less],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_greater_equal,
        "1>=2",
        vec![Op::Constant(0), Op::Constant(1), Op::Less, Op::Not],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_greater,
        "1>2",
        vec![Op::Constant(0), Op::Constant(1), Op::Greater],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
}