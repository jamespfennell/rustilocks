use crate::chunk;
use crate::chunk::Op;
use crate::error::*;
use crate::scanner::{self, Token, TokenType};
use crate::value::loxstring::LoxString;
use crate::value::Value;

pub fn compile(src: &str) -> Result<chunk::Chunk, Box<CompilationError>> {
    let mut compiler = Compiler {
        scanner: scanner::Scanner::new(src),
        chunk: Default::default(),
        locals: vec![],
        scope_depth: 0,
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
    locals: Vec<Local>,
    scope_depth: usize,
}

#[derive(Clone, Debug)]
struct Local {
    name: LoxString,
    depth: usize,
    initialized: bool,
}

impl<'a> Compiler<'a> {
    fn expression(&mut self) -> Result<(), Box<CompilationError>> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn declaration(&mut self) -> Result<(), Box<CompilationError>> {
        let next = match self.scanner.peek()? {
            None => return Ok(()),
            Some(next) => next,
        };
        match next.token_type {
            TokenType::Var => {
                self.scanner.consume()?;
                let name_token =
                    consume_specific_token(&mut self.scanner, TokenType::Identifier, |_| {
                        CompilationErrorKind::ExpectedIdentifier
                    })?;
                let name = self.chunk.string_interner.intern_ref(name_token.source);

                let define_global_op = if self.scope_depth == 0 {
                    // Global var declarations
                    let constant_i = self.add_constant(name_token, Value::String(name))?;
                    Some(Op::DefineGlobal(constant_i))
                } else {
                    // Local var declarations
                    // First we check that this is not a redeclaration.
                    for local in self.locals.iter().rev() {
                        if local.depth < self.scope_depth {
                            break;
                        }
                        if local.name == name {
                            return Err(Box::new(CompilationError {
                                line_number: self.scanner.line_number(),
                                at: name_token.source.into(),
                                kind: CompilationErrorKind::LocalRedeclared,
                            }));
                        }
                    }
                    if self.locals.len() == 256 {
                        return Err(Box::new(CompilationError {
                            line_number: self.scanner.line_number(),
                            at: "".into(),
                            kind: CompilationErrorKind::TooManyLocals,
                        }));
                    }
                    self.locals.push(Local {
                        name,
                        depth: self.scope_depth,
                        initialized: false,
                    });
                    None
                };

                match self.scanner.peek()? {
                    Some(Token {
                        token_type: TokenType::Equal,
                        ..
                    }) => {
                        self.scanner.consume()?;
                        self.expression()?
                    }
                    _ => self.emit_op(Op::Nil),
                };
                consume_specific_token(&mut self.scanner, TokenType::Semicolon, |_at| {
                    CompilationErrorKind::Todo(1)
                })?;

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

                Ok(())
            }
            _ => self.statement(),
        }
    }

    fn statement(&mut self) -> Result<(), Box<CompilationError>> {
        let next = match self.scanner.peek()? {
            None => return Ok(()),
            Some(next) => next,
        };
        match next.token_type {
            TokenType::Print => {
                self.scanner.consume()?;
                self.expression()?;
                consume_specific_token(&mut self.scanner, TokenType::Semicolon, |_| {
                    CompilationErrorKind::Todo(2)
                })?;
                self.emit_op(Op::Print);
            }
            TokenType::LeftBrace => {
                self.begin_scope();
                self.scanner.consume()?;
                self.block(next)?;
                self.end_scope();
            }
            TokenType::If => {
                self.scanner.consume()?;
                consume_specific_token(&mut self.scanner, TokenType::LeftParen, |_| {
                    CompilationErrorKind::Todo(5)
                })?;
                self.expression()?;
                consume_specific_token(&mut self.scanner, TokenType::RightParen, |_| {
                    CompilationErrorKind::Todo(6)
                })?;

                let jump_over_if = Jump::new(self, Op::JumpIfFalse);
                // The JumpIfFalse op leaves the true value on the top of the stack
                // if the jump is not taken. So we need to pop it off.
                self.emit_op(Op::Pop);
                self.statement()?;
                let jump_over_else = Jump::new(self, Op::Jump);
                jump_over_if.jump_to_here(self);
                // The JumpIfFalse op leaves the false value on the top of the stack
                // if the jump is taken. So we need to pop it off.
                self.emit_op(Op::Pop);
                if let Some(Token {
                    token_type: TokenType::Else,
                    ..
                }) = self.scanner.peek()?
                {
                    self.scanner.consume()?;
                    self.statement()?;
                };
                jump_over_else.jump_to_here(self);
            }
            TokenType::While => {
                self.scanner.consume()?;
                let while_block_start = self.chunk.bytecode.len();
                consume_specific_token(&mut self.scanner, TokenType::LeftParen, |_| {
                    CompilationErrorKind::Todo(7)
                })?;
                self.expression()?;
                consume_specific_token(&mut self.scanner, TokenType::RightParen, |_| {
                    CompilationErrorKind::Todo(8)
                })?;
                let jump_over_while = Jump::new(self, Op::JumpIfFalse);
                self.emit_op(Op::Pop);
                self.statement()?;
                self.jump_back(while_block_start);
                jump_over_while.jump_to_here(self);
                self.emit_op(Op::Pop);
            }
            TokenType::For => {
                self.scanner.consume()?;
                self.begin_scope();
                consume_specific_token(&mut self.scanner, TokenType::LeftParen, |_| {
                    CompilationErrorKind::Todo(9)
                })?;

                // initializer
                match self.scanner.peek()?.map(|t| t.token_type) {
                    Some(TokenType::Var) => self.declaration()?,
                    Some(TokenType::Semicolon) => {
                        // empty initializer
                        self.scanner.consume()?;
                    }
                    _ => {
                        // TODO: replace with expression statement?
                        self.expression()?;
                        consume_specific_token(&mut self.scanner, TokenType::Semicolon, |_| {
                            CompilationErrorKind::Todo(10)
                        })?;
                        self.emit_op(Op::Pop);
                    }
                }

                // condition
                let condition_start = self.chunk.bytecode.len();
                let jump_if_false = match self.scanner.peek()?.map(|t| t.token_type) {
                    Some(TokenType::Semicolon) => {
                        // empty condition
                        self.scanner.consume()?;
                        None
                    }
                    _ => {
                        self.expression()?;
                        let jump_if_false = Jump::new(self, Op::JumpIfFalse);
                        self.emit_op(Op::Pop);
                        consume_specific_token(&mut self.scanner, TokenType::Semicolon, |_| {
                            CompilationErrorKind::Todo(10)
                        })?;
                        Some(jump_if_false)
                    }
                };
                let jump_if_true = Jump::new(self, Op::Jump);

                // increment
                let incrementent_start = self.chunk.bytecode.len();
                match self.scanner.peek()?.map(|t| t.token_type) {
                    Some(TokenType::RightParen) => {
                        // empty increment
                        self.scanner.consume()?;
                    }
                    _ => {
                        self.expression()?;
                        self.emit_op(Op::Pop);
                        consume_specific_token(&mut self.scanner, TokenType::RightParen, |_| {
                            CompilationErrorKind::Todo(13)
                        })?;
                    }
                }
                self.jump_back(condition_start);

                // for loop block
                jump_if_true.jump_to_here(self);
                self.statement()?;
                self.jump_back(incrementent_start);
                if let Some(jump_if_false) = jump_if_false {
                    jump_if_false.jump_to_here(self);
                    self.emit_op(Op::Pop);
                }
                self.end_scope();
            }
            // Expression statement
            _ => {
                self.expression()?;
                consume_specific_token(&mut self.scanner, TokenType::Semicolon, |_| {
                    CompilationErrorKind::Todo(3)
                })?;
                self.emit_op(Op::Pop);
            }
        }
        Ok(())
    }

    fn block(&mut self, opening_token: Token<'a>) -> Result<(), Box<CompilationError>> {
        loop {
            match self.scanner.peek()? {
                None => {
                    return Err(Box::new(CompilationError {
                        line_number: opening_token.line_number,
                        at: "".into(),
                        kind: CompilationErrorKind::UnclosedBlock,
                    }))
                }
                Some(Token {
                    token_type: TokenType::RightBrace,
                    ..
                }) => {
                    self.scanner.consume()?;
                    break;
                }
                _ => {
                    self.declaration()?;
                }
            }
        }
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), Box<CompilationError>> {
        let token = match self.scanner.next()? {
            None => return Ok(()),
            Some(token) => token,
        };
        let (prefix_rule, _, _) = get_rules(token.token_type);
        let prefix_rule = match prefix_rule {
            None => {
                return Err(CompilationError::new(
                    token,
                    CompilationErrorKind::ExpectedExpression,
                ))
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
        // By this point we've compiled a full expression. The only case where we can assign to
        // the expression is if it's an identifier. In this case, the prefix_rule above is prefix_variable,
        // and that function consumed the trailing equals token. Thus if there is an equals sign here
        // it means that code path wasn't taken, and the expression cannot be assigned to.
        if can_assign {
            if let Some(token) = match_token_type(&mut self.scanner, TokenType::Equal)? {
                return Err(CompilationError::new(
                    token,
                    CompilationErrorKind::InvalidAssignmentTarget,
                ));
            }
        }
        Ok(())
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

    fn add_constant(
        &mut self,
        token: Token<'a>,
        constant: Value,
    ) -> Result<u8, Box<CompilationError>> {
        for (i, existing_constant) in self.chunk.constants.iter().enumerate() {
            if constant == *existing_constant {
                return Ok(i.try_into().expect("there are only 256 constants"));
            }
        }
        let i = self.chunk.constants.len();
        self.chunk.constants.push(constant);
        match i.try_into() {
            Ok(i) => Ok(i),
            Err(_) => Err(Box::new(CompilationError {
                line_number: token.line_number,
                at: "".into(),
                kind: CompilationErrorKind::TooManyConstants,
            })),
        }
    }

    fn jump_back(&mut self, to: usize) {
        let offset: u16 = self
            .chunk
            .bytecode
            .len()
            .checked_sub(to)
            .expect("bytecode array is non-decreasing in length")
            .try_into()
            .unwrap();
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
        let offset: u16 = c
            .chunk
            .bytecode
            .len()
            .checked_sub(self.block_start)
            .expect("bytecode array is non-decreasing in length")
            .try_into()
            .unwrap();
        let mut v = vec![];
        (self.op)(offset).write(&mut v);
        for (i, b) in v.iter().enumerate() {
            c.chunk.bytecode[i + self.op_idx] = *b;
        }
    }
}

fn consume_specific_token<'a>(
    scanner: &mut scanner::Scanner<'a>,
    token_type: TokenType,
    f: fn(token_or: &'a str) -> CompilationErrorKind,
) -> Result<Token<'a>, Box<CompilationError>> {
    match scanner.next()? {
        None => Err(Box::new(CompilationError {
            line_number: scanner.line_number(),
            at: "".into(),
            kind: f(""),
        })),
        Some(token) => {
            if token.token_type == token_type {
                Ok(token)
            } else {
                Err(Box::new(CompilationError {
                    line_number: scanner.line_number(),
                    at: token.source.into(),
                    kind: f(token.source),
                }))
            }
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

type ParseRule<'a> = fn(s: &mut Compiler<'a>, Token<'a>, bool) -> Result<(), Box<CompilationError>>;

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

fn infix_binary(c: &mut Compiler, token: Token, _: bool) -> Result<(), Box<CompilationError>> {
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
    c.parse_precedence(next_precedence)?;
    c.emit_op(op_1);
    if let Some(op_2) = op_2 {
        c.emit_op(op_2);
    }
    Ok(())
}

fn infix_or(c: &mut Compiler, _: Token, _: bool) -> Result<(), Box<CompilationError>> {
    let jump_if_false = Jump::new(c, Op::JumpIfFalse);
    let jump_if_true = Jump::new(c, Op::Jump);
    jump_if_false.jump_to_here(c);
    c.emit_op(Op::Pop);
    c.parse_precedence(Precedence::Or)?;
    jump_if_true.jump_to_here(c);
    Ok(())
}

fn infix_and(c: &mut Compiler, _: Token, _: bool) -> Result<(), Box<CompilationError>> {
    let jump = Jump::new(c, Op::JumpIfFalse);
    c.emit_op(Op::Pop);
    c.parse_precedence(Precedence::And)?;
    jump.jump_to_here(c);
    Ok(())
}

fn prefix_bang(c: &mut Compiler, _: Token, _: bool) -> Result<(), Box<CompilationError>> {
    c.parse_precedence(Precedence::Unary)?;
    c.emit_op(Op::Not);
    Ok(())
}

fn prefix_false(c: &mut Compiler, _: Token, _: bool) -> Result<(), Box<CompilationError>> {
    c.emit_op(Op::False);
    Ok(())
}

fn prefix_left_paren(c: &mut Compiler, _: Token, _: bool) -> Result<(), Box<CompilationError>> {
    c.expression()?;
    consume_specific_token(&mut c.scanner, TokenType::RightParen, |_| {
        CompilationErrorKind::Todo(4)
    })?;
    Ok(())
}

fn prefix_minus(c: &mut Compiler, _: Token, _: bool) -> Result<(), Box<CompilationError>> {
    c.parse_precedence(Precedence::Unary)?;
    c.emit_op(Op::Negate);
    Ok(())
}

fn prefix_nil(c: &mut Compiler, _: Token, _: bool) -> Result<(), Box<CompilationError>> {
    c.emit_op(Op::Nil);
    Ok(())
}

fn prefix_number<'a>(
    s: &mut Compiler<'a>,
    token: Token<'a>,
    _: bool,
) -> Result<(), Box<CompilationError>> {
    let d = match token.source.parse::<f64>() {
        Ok(d) => d,
        Err(_) => {
            return Err(CompilationError::new(
                token,
                CompilationErrorKind::InvalidNumber,
            ))
        }
    };
    let i = s.add_constant(token, Value::Number(d))?;
    s.emit_op(Op::Constant(i));
    Ok(())
}

fn prefix_string<'a>(
    c: &mut Compiler<'a>,
    token: Token<'a>,
    _: bool,
) -> Result<(), Box<CompilationError>> {
    // We need to trim the opening and closing quotation marks.
    let s = &token.source[1..token.source.len() - 1];
    let value = Value::String(c.chunk.string_interner.intern_ref(s));
    let i = c.add_constant(token, value)?;
    c.emit_op(Op::Constant(i));
    Ok(())
}

fn prefix_true(c: &mut Compiler, _: Token, _: bool) -> Result<(), Box<CompilationError>> {
    c.emit_op(Op::True);
    Ok(())
}

fn prefix_variable<'a>(
    c: &mut Compiler<'a>,
    token: Token<'a>,
    can_assign: bool,
) -> Result<(), Box<CompilationError>> {
    let name = c.chunk.string_interner.intern_ref(token.source);
    let mut local_index: Option<u8> = None;
    for (i, local) in c.locals.iter().enumerate().rev() {
        if local.name == name {
            // Check that the variable has not been declared
            if !local.initialized {
                return Err(Box::new(CompilationError {
                    line_number: token.line_number,
                    at: token.source.into(),
                    kind: CompilationErrorKind::LocalUninitialized,
                }));
            }
            local_index = Some(i.try_into().expect("no more than 256 locals"));
            break;
        }
    }
    let (get_op, set_op) = match local_index {
        // Global variable: the arg is the index in the constants table.
        None => {
            let i = c.add_constant(token, Value::String(name))?;
            (Op::GetGlobal(i), Op::SetGlobal(i))
        }
        // Local variable: the arg is the position on the stack.
        Some(i) => (Op::GetLocal(i), Op::SetLocal(i)),
    };
    if can_assign && match_token_type(&mut c.scanner, TokenType::Equal)?.is_some() {
        c.expression()?;
        c.emit_op(set_op);
    } else {
        c.emit_op(get_op);
    };
    Ok(())
}

fn match_token_type<'a>(
    scanner: &mut scanner::Scanner<'a>,
    token_type: TokenType,
) -> Result<Option<Token<'a>>, Box<CompilationError>> {
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
