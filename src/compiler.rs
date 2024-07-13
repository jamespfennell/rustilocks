use crate::chunk;
use crate::chunk::Op;
use crate::error::*;
use crate::scanner::{self, Token, TokenType};
use crate::value::Value;

pub fn compile(src: &str) -> Result<chunk::Chunk, Box<CompilationError>> {
    let mut compiler = Compiler {
        scanner: scanner::Scanner::new(src),
        chunk: chunk::Chunk::new(),
    };
    while compiler.scanner.peek()?.is_some() {
        compiler.declaration()?;
    }
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

    fn declaration(&mut self) -> Result<(), Box<CompilationError<'a>>> {
        let next = match self.scanner.peek()? {
            None => return Ok(()),
            Some(next) => next,
        };
        match next.token_type {
            TokenType::Var => {
                self.scanner.consume()?;
                let name = consume_specific_token(&mut self.scanner, TokenType::Identifier)?;
                let value = Value::String(self.chunk.string_interner.intern_ref(name.source));
                let constant_i = self.add_constant(name, value)?;
                match self.scanner.peek()? {
                    Some(Token {
                        token_type: TokenType::Equal,
                        source: _,
                    }) => {
                        self.scanner.consume()?;
                        self.expression()?
                    }
                    _ => self.emit_op(Op::Nil),
                };
                consume_specific_token(&mut self.scanner, TokenType::Semicolon)?;
                self.emit_op(Op::DefineGlobal(constant_i));
                Ok(())
            }
            _ => self.statement(),
        }
    }

    fn statement(&mut self) -> Result<(), Box<CompilationError<'a>>> {
        let next = match self.scanner.peek()? {
            None => return Ok(()),
            Some(next) => next,
        };
        match next.token_type {
            TokenType::Print => {
                self.scanner.consume()?;
                self.expression()?;
                consume_specific_token(&mut self.scanner, TokenType::Semicolon)?;
                self.emit_op(Op::Print);
                Ok(())
            }
            // Expression statement
            _ => {
                self.expression()?;
                consume_specific_token(&mut self.scanner, TokenType::Semicolon)?;
                self.emit_op(Op::Pop);
                Ok(())
            }
        }
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
        let can_assign = precedence <= Precedence::Assignment;
        prefix_rule(self, token, can_assign)?;

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
            infix_rule(self, token, can_assign)?;
        }
        if let Some(token) = match_token_type(&mut self.scanner, TokenType::Equal)? {
            if can_assign {
                return Err(Box::new(CompilationError::InvalidAssignmentTarget(token)));
            }
        }
        Ok(())
    }

    fn emit_op(&mut self, op: Op) {
        op.write(&mut self.chunk.bytecode);
    }

    fn add_constant(
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

fn consume_specific_token<'a>(
    scanner: &mut scanner::Scanner<'a>,
    token_type: TokenType,
) -> Result<Token<'a>, Box<CompilationError<'a>>> {
    match scanner.next()? {
        None => Err(Box::new(CompilationError::UnexpectedTokenType(
            None, token_type,
        ))),
        Some(token) => {
            if token.token_type == token_type {
                Ok(token)
            } else {
                Err(Box::new(CompilationError::UnexpectedTokenType(
                    Some(token),
                    token_type,
                )))
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Precedence {
    None = 0,
    Assignment = 1, // =
    // Or = 2,         // or
    // And = 3,        // and
    Equality = 4,   // == !=
    Comparison = 5, // < > <= >=
    Term = 6,       // + -
    Factor = 7,     // * /
    Unary = 8,      // ! -
                    // Call = 9,       // . ()
                    // Primary = 10,
}

type ParseRule<'a> =
    fn(s: &mut Compiler<'a>, Token<'a>, bool) -> Result<(), Box<CompilationError<'a>>>;

fn get_rules<'a>(
    token_type: TokenType,
) -> (Option<ParseRule<'a>>, Option<ParseRule<'a>>, Precedence) {
    match token_type {
        // Single-character tokens
        TokenType::LeftParen => (Some(prefix_left_paren), None, Precedence::None),
        TokenType::RightParen => (None, None, Precedence::None),
        TokenType::LeftBrace => (None, None, Precedence::None),
        TokenType::RightBrace => (None, None, Precedence::None),
        TokenType::Comma => (None, None, Precedence::None),
        TokenType::Dot => (None, None, Precedence::None),
        TokenType::Minus => (Some(prefix_minus), Some(infix_binary), Precedence::Term),
        TokenType::Plus => (None, Some(infix_binary), Precedence::Term),
        TokenType::Semicolon => (None, None, Precedence::None),
        TokenType::Slash => (None, Some(infix_binary), Precedence::Factor),
        TokenType::Star => (None, Some(infix_binary), Precedence::Factor),

        // One or two character tokens
        TokenType::Bang => (Some(prefix_bang), None, Precedence::Unary),
        TokenType::BangEqual => (None, Some(infix_binary), Precedence::Equality),
        TokenType::Equal => (None, None, Precedence::None),
        TokenType::EqualEqual => (None, Some(infix_binary), Precedence::Equality),
        TokenType::Greater => (None, Some(infix_binary), Precedence::Comparison),
        TokenType::GreaterEqual => (None, Some(infix_binary), Precedence::Comparison),
        TokenType::Less => (None, Some(infix_binary), Precedence::Comparison),
        TokenType::LessEqual => (None, Some(infix_binary), Precedence::Comparison),

        // Literals
        TokenType::Identifier => (Some(prefix_variable), None, Precedence::None),
        TokenType::String => (Some(prefix_string), None, Precedence::None),
        TokenType::Number => (Some(prefix_number), None, Precedence::None),

        // Keywords
        TokenType::And => (None, None, Precedence::None),
        TokenType::Class => (None, None, Precedence::None),
        TokenType::Else => (None, None, Precedence::None),
        TokenType::False => (Some(prefix_false), None, Precedence::None),
        TokenType::For => (None, None, Precedence::None),
        TokenType::Fun => (None, None, Precedence::None),
        TokenType::If => (None, None, Precedence::None),
        TokenType::Nil => (Some(prefix_nil), None, Precedence::None),
        TokenType::Or => (None, None, Precedence::None),
        TokenType::Print => (None, None, Precedence::None),
        TokenType::Return => (None, None, Precedence::None),
        TokenType::Super => (None, None, Precedence::None),
        TokenType::This => (None, None, Precedence::None),
        TokenType::True => (Some(prefix_true), None, Precedence::None),
        TokenType::Var => (None, None, Precedence::None),
        TokenType::While => (None, None, Precedence::None),
    }
}

// TODO: if this turns out to be the only infix function then we should refactor the code to
// not go through the rules at all. This way we could avoid double matching on the token type.
fn infix_binary<'a>(
    c: &mut Compiler<'a>,
    token: Token,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
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

fn prefix_bang<'a>(
    c: &mut Compiler<'a>,
    _: Token,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    c.parse_precendence(Precedence::Unary)?;
    c.emit_op(Op::Not);
    Ok(())
}

fn prefix_false<'a>(
    c: &mut Compiler<'a>,
    _: Token,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    c.emit_op(Op::False);
    Ok(())
}

fn prefix_left_paren<'a>(
    c: &mut Compiler<'a>,
    _: Token,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    c.expression()?;
    consume_specific_token(&mut c.scanner, TokenType::RightParen)?;
    Ok(())
}

fn prefix_minus<'a>(
    c: &mut Compiler<'a>,
    _: Token,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    c.parse_precendence(Precedence::Unary)?;
    c.emit_op(Op::Negate);
    Ok(())
}

fn prefix_nil<'a>(
    c: &mut Compiler<'a>,
    _: Token,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    c.emit_op(Op::Nil);
    Ok(())
}

fn prefix_number<'a>(
    s: &mut Compiler<'a>,
    token: Token<'a>,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    let d = match token.source.parse::<f64>() {
        Ok(d) => d,
        Err(_) => return Err(Box::new(CompilationError::InvalidNumber(token))),
    };
    let i = s.add_constant(token, Value::Number(d))?;
    s.emit_op(Op::Constant(i));
    Ok(())
}

fn prefix_string<'a>(
    c: &mut Compiler<'a>,
    token: Token<'a>,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    // We need to trim the opening and closing quotation marks.
    let s = &token.source[1..token.source.len() - 1];
    let value = Value::String(c.chunk.string_interner.intern_ref(s));
    let i = c.add_constant(token, value)?;
    c.emit_op(Op::Constant(i));
    Ok(())
}

fn prefix_true<'a>(
    c: &mut Compiler<'a>,
    _: Token,
    _: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    c.emit_op(Op::True);
    Ok(())
}

fn prefix_variable<'a>(
    c: &mut Compiler<'a>,
    token: Token<'a>,
    can_assign: bool,
) -> Result<(), Box<CompilationError<'a>>> {
    let name = Value::String(c.chunk.string_interner.intern_ref(token.source));
    let i = c.add_constant(token, name)?;
    if can_assign && match_token_type(&mut c.scanner, TokenType::Equal)?.is_some() {
        c.expression()?;
        c.emit_op(Op::SetGlobal(i));
    } else {
        c.emit_op(Op::GetGlobal(i));
    };
    Ok(())
}

fn match_token_type<'a>(
    scanner: &mut scanner::Scanner<'a>,
    token_type: TokenType,
) -> Result<Option<Token<'a>>, Box<CompilationError<'a>>> {
    let next_token = scanner.peek()?;
    if next_token.map(|t| t.token_type) == Some(token_type) {
        scanner.consume()?;
        Ok(next_token)
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use crate::value::loxstring::Interner;

    use super::*;

    fn no_op_constants_preprocesser(_: &mut Interner, v: Vec<Value>) -> Vec<Value> {
        v
    }

    fn string_constants_preprocesser(string_interner: &mut Interner, v: Vec<&str>) -> Vec<Value> {
        let mut out = vec![];
        for s in v {
            out.push(Value::String(string_interner.intern_ref(s)));
        }
        out
    }

    macro_rules! compiler_test {
        ($name: ident, $input: expr, $want_ops: expr, $want_constants: expr) => {
            compiler_test!(
                $name,
                $input,
                $want_ops,
                $want_constants,
                no_op_constants_preprocesser
            );
        };
        ($name: ident, $input: expr, $want_ops: expr, $want_constants: expr, $constants_preprocesser: ident) => {
            #[test]
            fn $name() {
                let mut want_ops = $want_ops;
                want_ops.push(Op::Return);

                let mut input: String = $input.into();
                input.push(';');
                let mut chunk = compile(&input).unwrap();

                assert_eq!(
                    chunk.constants,
                    $constants_preprocesser(&mut chunk.string_interner, $want_constants)
                );

                let got_ops = chunk.convert_bytecode_to_ops().unwrap();
                assert_eq!(got_ops, want_ops);
            }
        };
    }

    compiler_test!(
        prefix_minus,
        "-123",
        vec![Op::Constant(0), Op::Negate, Op::Pop],
        vec![Value::Number(123.0)]
    );
    compiler_test!(
        infix_minus,
        "1-2",
        vec![Op::Constant(0), Op::Constant(1), Op::Subtract, Op::Pop],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        infix_plus,
        "1+2",
        vec![Op::Constant(0), Op::Constant(1), Op::Add, Op::Pop],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        infix_slash,
        "1/2",
        vec![Op::Constant(0), Op::Constant(1), Op::Divide, Op::Pop],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        infix_star,
        "1*2",
        vec![Op::Constant(0), Op::Constant(1), Op::Multiply, Op::Pop],
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
            Op::Add,
            Op::Pop
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
            Op::Add,
            Op::Pop
        ],
        vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]
    );
    compiler_test!(
        negation_and_multiplication,
        "1*-2",
        vec![
            Op::Constant(0),
            Op::Constant(1),
            Op::Negate,
            Op::Multiply,
            Op::Pop
        ],
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
            Op::Multiply,
            Op::Pop
        ],
        vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]
    );
    compiler_test!(prefix_true, "true", vec![Op::True, Op::Pop], vec![]);
    compiler_test!(prefix_false, "false", vec![Op::False, Op::Pop], vec![]);
    compiler_test!(prefix_nil, "nil", vec![Op::Nil, Op::Pop], vec![]);
    compiler_test!(boolean, "!true", vec![Op::True, Op::Not, Op::Pop], vec![]);
    compiler_test!(
        prefix_bang_equal,
        "1!=2",
        vec![
            Op::Constant(0),
            Op::Constant(1),
            Op::Equal,
            Op::Not,
            Op::Pop
        ],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_equal_equal,
        "1==2",
        vec![Op::Constant(0), Op::Constant(1), Op::Equal, Op::Pop],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_less_equal,
        "1<=2",
        vec![
            Op::Constant(0),
            Op::Constant(1),
            Op::Greater,
            Op::Not,
            Op::Pop
        ],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_less,
        "1<2",
        vec![Op::Constant(0), Op::Constant(1), Op::Less, Op::Pop],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        prefix_greater_equal,
        "1>=2",
        vec![Op::Constant(0), Op::Constant(1), Op::Less, Op::Not, Op::Pop],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );

    compiler_test!(
        prefix_greater,
        "1>2",
        vec![Op::Constant(0), Op::Constant(1), Op::Greater, Op::Pop],
        vec![Value::Number(1.0), Value::Number(2.0)]
    );
    compiler_test!(
        less_than_inequality,
        "-2>1",
        vec![
            Op::Constant(0),
            Op::Negate,
            Op::Constant(1),
            Op::Greater,
            Op::Pop
        ],
        vec![Value::Number(2.0), Value::Number(1.0)]
    );
    compiler_test!(
        bang_comparison,
        "! true == false",
        vec![Op::True, Op::Not, Op::False, Op::Equal, Op::Pop],
        vec![]
    );
    compiler_test!(
        prefix_string,
        "\"hello\" + \"world\"",
        vec![Op::Constant(0), Op::Constant(1), Op::Add, Op::Pop],
        vec!["hello", "world"],
        string_constants_preprocesser
    );
    compiler_test!(
        new_variable,
        "var hello = \"world\"",
        vec![Op::Constant(1), Op::DefineGlobal(0)],
        vec!["hello", "world"],
        string_constants_preprocesser
    );
    compiler_test!(
        new_nil_variable,
        "var hello",
        vec![Op::Nil, Op::DefineGlobal(0)],
        vec!["hello"],
        string_constants_preprocesser
    );
    compiler_test!(
        get_variable,
        "hello + world",
        vec![Op::GetGlobal(0), Op::GetGlobal(1), Op::Add, Op::Pop],
        vec!["hello", "world"],
        string_constants_preprocesser
    );
    compiler_test!(
        overwrite_variable,
        "hello = \"world\"",
        vec![Op::Constant(1), Op::SetGlobal(0), Op::Pop],
        vec!["hello", "world"],
        string_constants_preprocesser
    );
}
