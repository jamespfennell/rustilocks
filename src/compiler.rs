use crate::chunk;
use crate::chunk::Op;
use crate::error;
use crate::error::*;
use crate::scanner::{self, Token, TokenType};
use crate::value::loxstring::LoxString;
use crate::value::Value;

pub fn compile(src: &str) -> Result<chunk::Chunk, Vec<CompilationError>> {
    let mut compiler = Compiler {
        scanner: scanner::Scanner::new(src),
        scanner_current: None,
        scanner_previous: None,
        chunk: Default::default(),
        locals: vec![],
        scope_depth: 0,
        errors: Default::default(),
    };
    while compiler.scanner_peek().is_some() {
        compiler.declaration();
    }
    compiler.emit_op(Op::Return);
    compiler.errors.propagate()?;
    Ok(compiler.chunk)
}

struct Compiler<'a> {
    scanner: scanner::Scanner<'a>,
    scanner_current: Option<Token<'a>>,
    scanner_previous: Option<Token<'a>>,
    chunk: chunk::Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
    errors: error::Accumulator,
}

#[derive(Clone, Debug)]
struct Local {
    name: LoxString,
    depth: usize,
    initialized: bool,
}

impl<'a> Compiler<'a> {
    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    fn declaration(&mut self) {
        let next = match self.scanner_peek() {
            None => return,
            Some(next) => next,
        };
        match next.token_type {
            TokenType::Var => {
                self.scanner_consume();
                let name_raw = self.consume(
                    TokenType::Identifier,
                    CompilationErrorKind::ExpectedIdentifier,
                );
                let name = self.chunk.string_interner.intern_ref(name_raw);

                let define_global_op = if self.scope_depth == 0 {
                    // Global var declarations
                    let constant_i = self.add_constant(Value::String(name));
                    Some(Op::DefineGlobal(constant_i))
                } else {
                    // Local var declarations
                    // First we check that this is not a redeclaration.
                    for local in self.locals.iter().rev() {
                        if local.depth < self.scope_depth {
                            break;
                        }
                        if local.name == name {
                            self.errors.add(CompilationError {
                                line_number: self.scanner.line_number(),
                                at: name_raw.into(),
                                kind: CompilationErrorKind::LocalRedeclared,
                            });
                        }
                    }
                    if self.locals.len() == 256 {
                        self.errors.add(CompilationError {
                            line_number: self.scanner.line_number(),
                            at: "".into(),
                            kind: CompilationErrorKind::TooManyLocals,
                        });
                    }
                    self.locals.push(Local {
                        name,
                        depth: self.scope_depth,
                        initialized: false,
                    });
                    None
                };

                match self.scanner_peek() {
                    Some(Token {
                        token_type: TokenType::Equal,
                        ..
                    }) => {
                        self.scanner_consume();
                        self.expression();
                    }
                    _ => self.emit_op(Op::Nil),
                };
                self.consume(TokenType::Semicolon, CompilationErrorKind::Todo(1));

                match define_global_op {
                    None => {
                        self.locals
                            .last_mut()
                            .expect("the local being defined is on top of the locals stack")
                            .initialized = true;
                    }
                    Some(op) => {
                        self.emit_op(op);
                    }
                }
            }
            _ => self.statement(),
        }

        // Synchronize
        if self.errors.panic_mode() {
            loop {
                if let Some(Token {
                    token_type: TokenType::Semicolon,
                    ..
                }) = self.scanner_last()
                {
                    break;
                }
                let next = match self.scanner_peek() {
                    None => break,
                    Some(next) => next,
                };
                match next.token_type {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return => break,
                    _ => {}
                }
                self.scanner_consume();
            }
            self.errors.reset_panic_mode();
        }
    }

    fn statement(&mut self) {
        let next = match self.scanner_peek() {
            None => return,
            Some(next) => next,
        };
        match next.token_type {
            TokenType::Print => {
                self.scanner_consume();
                self.expression();
                self.consume(TokenType::Semicolon, CompilationErrorKind::ExpectedSemicolonAfterValue);
                self.emit_op(Op::Print);
            }
            TokenType::LeftBrace => {
                self.begin_scope();
                self.scanner_consume();
                self.block(next);
                self.end_scope();
            }
            TokenType::If => {
                self.scanner_consume();
                self.consume(TokenType::LeftParen, CompilationErrorKind::Todo(5));
                self.expression();
                self.consume(TokenType::RightParen, CompilationErrorKind::Todo(6));

                let jump_over_if = Jump::new(self, Op::JumpIfFalse);
                // The JumpIfFalse op leaves the true value on the top of the stack
                // if the jump is not taken. So we need to pop it off.
                self.emit_op(Op::Pop);
                self.statement();
                let jump_over_else = Jump::new(self, Op::Jump);
                jump_over_if.jump_to_here(self);
                // The JumpIfFalse op leaves the false value on the top of the stack
                // if the jump is taken. So we need to pop it off.
                self.emit_op(Op::Pop);
                if let Some(Token {
                    token_type: TokenType::Else,
                    ..
                }) = self.scanner_peek()
                {
                    self.scanner_consume();
                    self.statement();
                };
                jump_over_else.jump_to_here(self);
            }
            TokenType::While => {
                self.scanner_consume();
                let while_block_start = self.chunk.bytecode.len();
                self.consume(TokenType::LeftParen, CompilationErrorKind::Todo(7));
                self.expression();
                self.consume(TokenType::RightParen, CompilationErrorKind::Todo(8));
                let jump_over_while = Jump::new(self, Op::JumpIfFalse);
                self.emit_op(Op::Pop);
                self.statement();
                self.jump_back(while_block_start);
                jump_over_while.jump_to_here(self);
                self.emit_op(Op::Pop);
            }
            TokenType::For => {
                self.scanner_consume();
                self.begin_scope();
                self.consume(TokenType::LeftParen, CompilationErrorKind::Todo(9));

                // initializer
                match self.scanner_peek().map(|t| t.token_type) {
                    Some(TokenType::Semicolon) => {
                        // empty initializer
                        self.scanner_consume();
                    }
                    Some(TokenType::Var) => self.declaration(),
                    _ => self.expression_statement(),
                }

                // condition
                let condition_start = self.chunk.bytecode.len();
                let jump_if_false = match self.scanner_peek().map(|t| t.token_type) {
                    Some(TokenType::Semicolon) => {
                        // empty condition
                        self.scanner_consume();
                        None
                    }
                    _ => {
                        self.expression();
                        let jump_if_false = Jump::new(self, Op::JumpIfFalse);
                        self.emit_op(Op::Pop);
                        self.consume(TokenType::Semicolon, CompilationErrorKind::Todo(10));
                        Some(jump_if_false)
                    }
                };
                let jump_if_true = Jump::new(self, Op::Jump);

                // increment
                let increment_start = self.chunk.bytecode.len();
                match self.scanner_peek().map(|t| t.token_type) {
                    Some(TokenType::RightParen) => {
                        // empty increment
                        self.scanner_consume();
                    }
                    _ => {
                        self.expression();
                        self.emit_op(Op::Pop);
                        self.consume(TokenType::RightParen, CompilationErrorKind::Todo(13));
                    }
                }
                self.jump_back(condition_start);

                // for loop block
                jump_if_true.jump_to_here(self);
                self.statement();
                self.jump_back(increment_start);
                if let Some(jump_if_false) = jump_if_false {
                    jump_if_false.jump_to_here(self);
                    self.emit_op(Op::Pop);
                }
                self.end_scope();
            }
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(
            TokenType::Semicolon,
            CompilationErrorKind::ExpectedSemicolonAfterExpression,
        );
        self.emit_op(Op::Pop);
    }

    fn block(&mut self, opening_token: Token<'a>) {
        loop {
            match self.scanner_peek() {
                None => {
                    self.errors.add(CompilationError {
                        line_number: opening_token.line_number,
                        at: "".into(),
                        kind: CompilationErrorKind::UnclosedBlock,
                    });
                    return;
                }
                Some(Token {
                    token_type: TokenType::RightBrace,
                    ..
                }) => {
                    self.scanner_consume();
                    break;
                }
                _ => {
                    self.declaration();
                }
            }
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        let token = match self.scanner_next() {
            None => return,
            Some(token) => token,
        };
        let (prefix_rule, _, _) = get_rules(token.token_type);
        let prefix_rule = match prefix_rule {
            None => {
                self.errors.add(CompilationError::new(
                    token,
                    CompilationErrorKind::ExpectedExpression,
                ));
                return;
            }
            Some(prefix_rule) => prefix_rule,
        };
        let can_assign = precedence <= Precedence::Assignment;
        prefix_rule(self, token, can_assign);

        loop {
            let token = match self.scanner_peek() {
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
            self.scanner_consume();
            infix_rule(self, token, can_assign);
        }
        // By this point we've compiled a full expression. The only case where we can assign to
        // the expression is if it's an identifier. In this case, the prefix_rule above is prefix_variable,
        // and that function consumed the trailing equals token. Thus if there is an equals sign here
        // it means that code path wasn't taken, and the expression cannot be assigned to.
        if can_assign {
            if let Some(token) = match_token_type(self, TokenType::Equal) {
                self.errors.add(CompilationError::new(
                    token,
                    CompilationErrorKind::InvalidAssignmentTarget,
                ));
            }
        }
    }

    fn emit_op(&mut self, op: Op) {
        let l = self.chunk.bytecode.len();
        op.write(&mut self.chunk.bytecode);
        // TODO: optimize the line numbers array. Two ideas: store line numbers
        // as diffs, and use a variable encoding.
        for _ in l..self.chunk.bytecode.len() {
            self.chunk.line_numbers.push(self.scanner.line_number());
        }
    }

    fn add_constant(&mut self, constant: Value) -> u8 {
        for (i, existing_constant) in self.chunk.constants.iter().enumerate() {
            if constant == *existing_constant {
                return i.try_into().expect("there are only 256 constants");
            }
        }
        let i = self.chunk.constants.len();
        self.chunk.constants.push(constant);
        match i.try_into() {
            Ok(i) => i,
            Err(_) => {
                self.errors.add(CompilationError {
                    line_number: self.scanner.line_number(),
                    at: "".into(),
                    kind: CompilationErrorKind::TooManyConstants,
                });
                0
            }
        }
    }

    fn jump_back(&mut self, to: usize) {
        let offset: u16 = match self
            .chunk
            .bytecode
            .len()
            .checked_sub(to)
            .expect("bytecode array is non-decreasing in length")
            .try_into()
        {
            Ok(offset) => offset,
            Err(_) => {
                self.errors.add(CompilationError {
                    line_number: self.scanner.line_number(),
                    at: "}".into(),
                    kind: CompilationErrorKind::LoopTooLarge,
                });
                0
            }
        };
        self.emit_op(Op::JumpBack(offset));
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        while let Some(local) = self.locals.last().cloned() {
            if local.depth < self.scope_depth {
                break;
            }
            self.emit_op(Op::Pop);
            self.locals.pop();
        }
        self.scope_depth -= 1;
    }

    fn consume(&mut self, token_type: TokenType, error_kind: CompilationErrorKind) -> &'a str {
        match self.scanner_peek() {
            None => {
                self.errors.add(CompilationError {
                    line_number: self.scanner.line_number(),
                    at: "".into(),
                    kind: error_kind,
                });
                ""
            }
            Some(token) => {
                if token.token_type == token_type {
                    self.scanner_consume();
                    token.source
                } else {
                    self.errors.add(CompilationError {
                        line_number: self.scanner.line_number(),
                        at: token.source.into(),
                        kind: error_kind,
                    });
                    ""
                }
            }
        }
    }

    fn scanner_next(&mut self) -> Option<Token<'a>> {
        let token_or = match self.scanner_current.take() {
            Some(token) => Some(token),
            None => loop {
                match self.scanner.next() {
                    Ok(token_or) => break token_or,
                    Err(error) => self.errors.add(error),
                }
            },
        };
        self.scanner_previous = token_or;
        token_or
    }

    fn scanner_peek(&mut self) -> Option<Token<'a>> {
        let token_or = match self.scanner_current {
            Some(token) => Some(token),
            None => loop {
                match self.scanner.next() {
                    Ok(token_or) => break token_or,
                    Err(error) => self.errors.add(error),
                }
            },
        };
        self.scanner_current = token_or;
        token_or
    }

    fn scanner_last(&self) -> Option<Token<'a>> {
        self.scanner_previous
    }

    fn scanner_consume(&mut self) {
        self.scanner_next();
    }
}

struct Jump {
    op: fn(u16) -> Op,
    op_idx: usize,
    block_start: usize,
}

impl Jump {
    fn new(c: &mut Compiler, op: fn(u16) -> Op) -> Self {
        let op_idx = c.chunk.bytecode.len();
        c.emit_op(op(0));
        Self {
            op,
            op_idx,
            block_start: c.chunk.bytecode.len(),
        }
    }
    fn jump_to_here(self, c: &mut Compiler) {
        let offset: u16 = match c
            .chunk
            .bytecode
            .len()
            .checked_sub(self.block_start)
            .expect("bytecode array is non-decreasing in length")
            .try_into()
        {
            Ok(offset) => offset,
            Err(_) => {
                c.errors.add(CompilationError {
                    line_number: c.scanner.line_number(),
                    at: "}".into(),
                    kind: CompilationErrorKind::LoopTooLarge,
                });
                0
            }
        };
        let mut v = vec![];
        (self.op)(offset).write(&mut v);
        for (i, b) in v.iter().enumerate() {
            c.chunk.bytecode[i + self.op_idx] = *b;
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
                    // Call = 9,       // . ()
                    // Primary = 10,
}

type ParseRule<'a> = fn(s: &mut Compiler<'a>, Token<'a>, bool);

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
        TokenType::And => (None, Some(infix_and), Precedence::And),
        TokenType::Class => (None, None, Precedence::None),
        TokenType::Else => (None, None, Precedence::None),
        TokenType::False => (Some(prefix_false), None, Precedence::None),
        TokenType::For => (None, None, Precedence::None),
        TokenType::Fun => (None, None, Precedence::None),
        TokenType::If => (None, None, Precedence::None),
        TokenType::Nil => (Some(prefix_nil), None, Precedence::None),
        TokenType::Or => (None, Some(infix_or), Precedence::Or),
        TokenType::Print => (None, None, Precedence::None),
        TokenType::Return => (None, None, Precedence::None),
        TokenType::Super => (None, None, Precedence::None),
        TokenType::This => (None, None, Precedence::None),
        TokenType::True => (Some(prefix_true), None, Precedence::None),
        TokenType::Var => (None, None, Precedence::None),
        TokenType::While => (None, None, Precedence::None),
    }
}

fn infix_binary(c: &mut Compiler, token: Token, _: bool) {
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
    c.parse_precedence(next_precedence);
    c.emit_op(op_1);
    if let Some(op_2) = op_2 {
        c.emit_op(op_2);
    }
}

fn infix_or(c: &mut Compiler, _: Token, _: bool) {
    let jump_if_false = Jump::new(c, Op::JumpIfFalse);
    let jump_if_true = Jump::new(c, Op::Jump);
    jump_if_false.jump_to_here(c);
    c.emit_op(Op::Pop);
    c.parse_precedence(Precedence::Or);
    jump_if_true.jump_to_here(c);
}

fn infix_and(c: &mut Compiler, _: Token, _: bool) {
    let jump = Jump::new(c, Op::JumpIfFalse);
    c.emit_op(Op::Pop);
    c.parse_precedence(Precedence::And);
    jump.jump_to_here(c);
}

fn prefix_bang(c: &mut Compiler, _: Token, _: bool) {
    c.parse_precedence(Precedence::Unary);
    c.emit_op(Op::Not);
}

fn prefix_false(c: &mut Compiler, _: Token, _: bool) {
    c.emit_op(Op::False);
}

fn prefix_left_paren(c: &mut Compiler, _: Token, _: bool) {
    c.expression();
    c.consume(TokenType::RightParen, CompilationErrorKind::Todo(4));
}

fn prefix_minus(c: &mut Compiler, _: Token, _: bool) {
    c.parse_precedence(Precedence::Unary);
    c.emit_op(Op::Negate);
}

fn prefix_nil(c: &mut Compiler, _: Token, _: bool) {
    c.emit_op(Op::Nil);
}

fn prefix_number<'a>(s: &mut Compiler<'a>, token: Token<'a>, _: bool) {
    let d = match token.source.parse::<f64>() {
        Ok(d) => d,
        Err(_) => {
            s.errors.add(CompilationError::new(
                token,
                CompilationErrorKind::InvalidNumber,
            ));
            0_f64
        }
    };
    let i = s.add_constant(Value::Number(d));
    s.emit_op(Op::Constant(i));
}

fn prefix_string<'a>(c: &mut Compiler<'a>, token: Token<'a>, _: bool) {
    // We need to trim the opening and closing quotation marks.
    let s = &token.source[1..token.source.len() - 1];
    let value = Value::String(c.chunk.string_interner.intern_ref(s));
    let i = c.add_constant(value);
    c.emit_op(Op::Constant(i));
}

fn prefix_true(c: &mut Compiler, _: Token, _: bool) {
    c.emit_op(Op::True);
}

fn prefix_variable<'a>(c: &mut Compiler<'a>, token: Token<'a>, can_assign: bool) {
    let name = c.chunk.string_interner.intern_ref(token.source);
    let mut local_index: Option<u8> = None;
    for (i, local) in c.locals.iter().enumerate().rev() {
        if local.name == name {
            // Check that the variable has not been declared
            if !local.initialized {
                c.errors.add(CompilationError {
                    line_number: token.line_number,
                    at: token.source.into(),
                    kind: CompilationErrorKind::LocalUninitialized,
                });
            }
            local_index = Some(i.try_into().expect("no more than 256 locals"));
            break;
        }
    }
    let (get_op, set_op) = match local_index {
        // Global variable: the arg is the index in the constants table.
        None => {
            let i = c.add_constant(Value::String(name));
            (Op::GetGlobal(i), Op::SetGlobal(i))
        }
        // Local variable: the arg is the position on the stack.
        Some(i) => (Op::GetLocal(i), Op::SetLocal(i)),
    };
    if can_assign && match_token_type(c, TokenType::Equal).is_some() {
        c.expression();
        c.emit_op(set_op);
    } else {
        c.emit_op(get_op);
    };
}

fn match_token_type<'a>(c: &mut Compiler<'a>, token_type: TokenType) -> Option<Token<'a>> {
    let next_token = c.scanner_peek();
    if next_token.map(|t| t.token_type) == Some(token_type) {
        c.scanner_next();
        next_token
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::loxstring::Interner;

    trait Constant {
        fn into_value(self, _: &mut Interner) -> Value;
    }
    impl Constant for f64 {
        fn into_value(self, _: &mut Interner) -> Value {
            Value::Number(self)
        }
    }
    impl Constant for &'static str {
        fn into_value(self, interner: &mut Interner) -> Value {
            Value::String(interner.intern_ref(self))
        }
    }

    macro_rules! compiler_tests {
        (
            $(
                (
                    $name: ident,
                    $input: expr,
                    $want_ops: expr,
                    [ $( $want_constant: expr ),* $(,)? ] $(,)?
                ),
            )+
        )   => {
        $(
            #[test]
            fn $name() {
                let mut want_ops = $want_ops;
                want_ops.push(Op::Return);

                let input: String = $input.into();
                #[allow(unused_mut)]
                let mut chunk = compile(&input).unwrap();

                let want_constants: Vec<Value> = vec![
                    $(
                        Constant::into_value( $want_constant, &mut chunk.string_interner),
                    )*
                ];

                assert_eq!(
                    chunk.constants,
                    want_constants,
                );

                let got_ops = chunk.convert_bytecode_to_ops().unwrap();
                assert_eq!(got_ops, want_ops);
            }
          )+
        };
    }

    compiler_tests!(
        (
            prefix_minus,
            "-123;",
            vec![Op::Constant(0), Op::Negate, Op::Pop],
            [123.0],
        ),
        (
            infix_minus,
            "1-2;",
            vec![Op::Constant(0), Op::Constant(1), Op::Subtract, Op::Pop],
            [1.0, 2.0],
        ),
        (
            infix_plus,
            "1+2;",
            vec![Op::Constant(0), Op::Constant(1), Op::Add, Op::Pop],
            [1.0, 2.0],
        ),
        (
            infix_slash,
            "1/2;",
            vec![Op::Constant(0), Op::Constant(1), Op::Divide, Op::Pop],
            [1.0, 2.0],
        ),
        (
            infix_star,
            "1*2;",
            vec![Op::Constant(0), Op::Constant(1), Op::Multiply, Op::Pop],
            [1.0, 2.0],
        ),
        (
            addition_and_multiplication_1,
            "1*2+3;",
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Multiply,
                Op::Constant(2),
                Op::Add,
                Op::Pop
            ],
            [1.0, 2.0, 3.0],
        ),
        (
            addition_and_multiplication_2,
            "1+2*3;",
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Constant(2),
                Op::Multiply,
                Op::Add,
                Op::Pop
            ],
            [1.0, 2.0, 3.0],
        ),
        (
            negation_and_multiplication,
            "1*-2;",
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Negate,
                Op::Multiply,
                Op::Pop
            ],
            [1.0, 2.0],
        ),
        (
            grouping_1,
            "1*(2+3);",
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Constant(2),
                Op::Add,
                Op::Multiply,
                Op::Pop
            ],
            [1.0, 2.0, 3.0],
        ),
        (prefix_true, "true;", vec![Op::True, Op::Pop], []),
        (prefix_false, "false;", vec![Op::False, Op::Pop], []),
        (prefix_nil, "nil;", vec![Op::Nil, Op::Pop], []),
        (boolean, "!true;", vec![Op::True, Op::Not, Op::Pop], []),
        (
            prefix_bang_equal,
            "1!=2;",
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Equal,
                Op::Not,
                Op::Pop
            ],
            [1.0, 2.0],
        ),
        (
            prefix_equal_equal,
            "1==2;",
            vec![Op::Constant(0), Op::Constant(1), Op::Equal, Op::Pop],
            [1.0, 2.0],
        ),
        (
            prefix_less_equal,
            "1<=2;",
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Greater,
                Op::Not,
                Op::Pop
            ],
            [1.0, 2.0],
        ),
        (
            prefix_less,
            "1<2;",
            vec![Op::Constant(0), Op::Constant(1), Op::Less, Op::Pop],
            [1.0, 2.0],
        ),
        (
            prefix_greater_equal,
            "1>=2;",
            vec![Op::Constant(0), Op::Constant(1), Op::Less, Op::Not, Op::Pop],
            [1.0, 2.0],
        ),
        (
            prefix_greater,
            "1>2;",
            vec![Op::Constant(0), Op::Constant(1), Op::Greater, Op::Pop],
            [1.0, 2.0],
        ),
        (
            less_than_inequality,
            "-2>1;",
            vec![
                Op::Constant(0),
                Op::Negate,
                Op::Constant(1),
                Op::Greater,
                Op::Pop
            ],
            [2.0, 1.0],
        ),
        (
            bang_comparison,
            "! true == false;",
            vec![Op::True, Op::Not, Op::False, Op::Equal, Op::Pop],
            []
        ),
        (
            prefix_string,
            "\"hello\" + \"world\";",
            vec![Op::Constant(0), Op::Constant(1), Op::Add, Op::Pop],
            ["hello", "world"],
        ),
        (
            global_var_bool,
            "var hello = false;",
            vec![Op::False, Op::DefineGlobal(0)],
            ["hello"],
        ),
        (
            global_var_number,
            "var hello = 6;",
            vec![Op::Constant(1), Op::DefineGlobal(0)],
            ["hello", 6.0],
        ),
        (
            global_var_string,
            "var hello = \"world\"; var hola = \"world\";",
            vec![
                Op::Constant(1),
                Op::DefineGlobal(0),
                Op::Constant(1),
                Op::DefineGlobal(2)
            ],
            ["hello", "world", "hola"],
        ),
        (
            new_nil_variable,
            "var hello;",
            vec![Op::Nil, Op::DefineGlobal(0)],
            ["hello"],
        ),
        (
            get_variable,
            "hello + world;",
            vec![Op::GetGlobal(0), Op::GetGlobal(1), Op::Add, Op::Pop],
            ["hello", "world"],
        ),
        (
            overwrite_variable,
            "hello = \"world\";",
            vec![Op::Constant(1), Op::SetGlobal(0), Op::Pop],
            ["hello", "world"],
        ),
        (
            local_var_number,
            "{ var local = 7; }",
            vec![Op::Constant(0), Op::Pop],
            [7.0],
        ),
        (
            local_var_mulitple,
            "{ var a = 1; var b = 2; var c = a + b; }",
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::GetLocal(0),
                Op::GetLocal(1),
                Op::Add,
                Op::Pop,
                Op::Pop,
                Op::Pop,
            ],
            [1.0, 2.0],
        ),
    );
}
