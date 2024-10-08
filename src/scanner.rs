use crate::error::{CompilationError, CompilationErrorKind};

pub struct Scanner<'a> {
    source: &'a str,
    line_number: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Scanner<'a> {
        Scanner {
            source: src,
            line_number: 1,
        }
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }
    pub fn next(&mut self) -> Result<Option<Token<'a>>, CompilationError> {
        self.skip_whitespace();
        let mut chars = self.source.chars();
        let c = match chars.next() {
            None => return Ok(None),
            Some(c) => c,
        };
        let (token_type, len) = match c {
            '(' => (TokenType::LeftParen, 1),
            ')' => (TokenType::RightParen, 1),
            '{' => (TokenType::LeftBrace, 1),
            '}' => (TokenType::RightBrace, 1),
            ';' => (TokenType::Semicolon, 1),
            ',' => (TokenType::Comma, 1),
            '.' => (TokenType::Dot, 1),
            '-' => (TokenType::Minus, 1),
            '+' => (TokenType::Plus, 1),
            '/' => (TokenType::Slash, 1),
            '*' => (TokenType::Star, 1),
            '!' => {
                if chars.next() == Some('=') {
                    (TokenType::BangEqual, 2)
                } else {
                    (TokenType::Bang, 1)
                }
            }
            '=' => {
                if chars.next() == Some('=') {
                    (TokenType::EqualEqual, 2)
                } else {
                    (TokenType::Equal, 1)
                }
            }
            '<' => {
                if chars.next() == Some('=') {
                    (TokenType::LessEqual, 2)
                } else {
                    (TokenType::Less, 1)
                }
            }
            '>' => {
                if chars.next() == Some('=') {
                    (TokenType::GreaterEqual, 2)
                } else {
                    (TokenType::Greater, 1)
                }
            }
            '"' => {
                let mut len = 1;
                loop {
                    match chars.next() {
                        None => {
                            self.source = "";
                            return Err(CompilationError {
                                line_number: self.line_number,
                                at: "".into(),
                                kind: CompilationErrorKind::UnterminatedString,
                            });
                        }
                        Some('"') => break,
                        Some('\n') => {
                            self.line_number += 1;
                            len += 1;
                        }
                        Some(c) => {
                            len += c.len_utf8();
                        }
                    }
                }
                (TokenType::String, len + 1)
            }
            '0'..='9' => {
                let mut len = 1;
                while let Some(c) = chars.next() {
                    match c {
                        '0'..='9' => {
                            len += 1;
                        }
                        '.' => {
                            len += 1;
                            for c in chars.by_ref() {
                                match c {
                                    '0'..='9' => {
                                        len += 1;
                                    }
                                    _ => break,
                                }
                            }
                        }
                        _ => break,
                    }
                }
                (TokenType::Number, len)
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut len = 1;
                for c in chars {
                    match c {
                        'A'..='Z' | 'a'..='z' | '_' | '0'..='9' => {
                            len += 1;
                        }
                        _ => break,
                    }
                }
                (identifier_token_type(&self.source[..len]), len)
            }
            c => {
                self.source = &self.source[c.len_utf8()..];
                return Err(CompilationError {
                    line_number: self.line_number,
                    at: c.into(),
                    kind: CompilationErrorKind::InvalidCharacter,
                });
            }
        };
        let token_text = &self.source[..len];
        self.source = &self.source[len..];
        Ok(Some(Token::new(token_type, self.line_number, token_text)))
    }

    fn skip_whitespace(&mut self) {
        let mut chars = self.source.chars();
        let mut bytes_to_skip = 0;
        while let Some(c) = chars.next() {
            match c {
                '\n' => {
                    self.line_number += 1;
                    bytes_to_skip += 1;
                }
                ' ' | '\r' | '\t' => {
                    bytes_to_skip += 1;
                }
                '/' => {
                    if chars.next() == Some('/') {
                        bytes_to_skip += 2;
                        for c in chars.by_ref() {
                            bytes_to_skip += c.len_utf8();
                            if c == '\n' {
                                self.line_number += 1;
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        self.source = &self.source[bytes_to_skip..];
    }
}

fn identifier_token_type(name: &str) -> TokenType {
    let check_keyword = |keyword, token_type| {
        if keyword == name {
            token_type
        } else {
            TokenType::Identifier
        }
    };
    let mut chars = name.chars();
    match chars.next() {
        Some('a') => check_keyword("and", TokenType::And),
        Some('c') => check_keyword("class", TokenType::Class),
        Some('e') => check_keyword("else", TokenType::Else),
        Some('f') => match chars.next() {
            Some('a') => check_keyword("false", TokenType::False),
            Some('o') => check_keyword("for", TokenType::For),
            Some('u') => check_keyword("fun", TokenType::Fun),
            _ => TokenType::Identifier,
        },
        Some('i') => check_keyword("if", TokenType::If),
        Some('n') => check_keyword("nil", TokenType::Nil),
        Some('o') => check_keyword("or", TokenType::Or),
        Some('p') => check_keyword("print", TokenType::Print),
        Some('r') => check_keyword("return", TokenType::Return),
        Some('s') => check_keyword("super", TokenType::Super),
        Some('t') => match chars.next() {
            Some('h') => check_keyword("this", TokenType::This),
            Some('r') => check_keyword("true", TokenType::True),
            _ => TokenType::Identifier,
        },
        Some('v') => check_keyword("var", TokenType::Var),
        Some('w') => check_keyword("while", TokenType::While),
        _ => TokenType::Identifier,
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub line_number: usize,
    pub source: &'a str,
}

impl<'a> Token<'a> {
    fn new(token_type: TokenType, line_number: usize, source: &'a str) -> Token<'a> {
        Token {
            token_type,
            line_number,
            source,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens
    LeftParen = 0,
    RightParen = 1,
    LeftBrace = 2,
    RightBrace = 3,
    Comma = 4,
    Dot = 5,
    Minus = 6,
    Plus = 7,
    Semicolon = 8,
    Slash = 9,
    Star = 10,

    // One or two character tokens
    Bang = 11,
    BangEqual = 12,
    Equal = 13,
    EqualEqual = 14,
    Greater = 15,
    GreaterEqual = 16,
    Less = 17,
    LessEqual = 18,

    // Literals
    Identifier = 19,
    String = 20,
    Number = 21,

    // Keywords
    And = 22,
    Class = 23,
    Else = 24,
    False = 25,
    For = 26,
    Fun = 27,
    If = 28,
    Nil = 29,
    Or = 30,
    Print = 31,
    Return = 32,
    Super = 33,
    This = 34,
    True = 35,
    Var = 36,
    While = 37,
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! scanner_tests {
      ( $(  ($name: ident, $input: expr, $output: expr), )* ) => {
        $(
            #[test]
            fn $name() {
                let mut scanner = Scanner::new($input);
                let mut got = vec![];
                while let Some(token) = scanner.next().expect("valid token") {
                    got.push(token);
                }
                assert_eq!($output, got);
            }
      )*
        };
    }

    scanner_tests!(
        (
            left_paren,
            "(",
            vec![Token::new(TokenType::LeftParen, 1, "(")]
        ),
        (
            right_paren,
            ")",
            vec![Token::new(TokenType::RightParen, 1, ")")]
        ),
        (
            left_brace,
            "{",
            vec![Token::new(TokenType::LeftBrace, 1, "{")]
        ),
        (
            right_brace,
            "}",
            vec![Token::new(TokenType::RightBrace, 1, "}")]
        ),
        (
            semicolon,
            ";",
            vec![Token::new(TokenType::Semicolon, 1, ";")]
        ),
        (comma, ",", vec![Token::new(TokenType::Comma, 1, ",")]),
        (dot, ".", vec![Token::new(TokenType::Dot, 1, ".")]),
        (minus, "-", vec![Token::new(TokenType::Minus, 1, "-")]),
        (plus, "+", vec![Token::new(TokenType::Plus, 1, "+")]),
        (slash, "/", vec![Token::new(TokenType::Slash, 1, "/")]),
        (star, "*", vec![Token::new(TokenType::Star, 1, "*")]),
        (bang, "!", vec![Token::new(TokenType::Bang, 1, "!")]),
        (
            bang_bang,
            "!!",
            vec![
                Token::new(TokenType::Bang, 1, "!"),
                Token::new(TokenType::Bang, 1, "!")
            ]
        ),
        (
            bang_equal,
            "!=",
            vec![Token::new(TokenType::BangEqual, 1, "!=")]
        ),
        (equal, "=", vec![Token::new(TokenType::Equal, 1, "=")]),
        (
            equal_bang,
            "=!",
            vec![
                Token::new(TokenType::Equal, 1, "="),
                Token::new(TokenType::Bang, 1, "!")
            ]
        ),
        (
            equal_equal,
            "==",
            vec![Token::new(TokenType::EqualEqual, 1, "==")]
        ),
        (less, "<", vec![Token::new(TokenType::Less, 1, "<")]),
        (
            less_less,
            "<<",
            vec![
                Token::new(TokenType::Less, 1, "<"),
                Token::new(TokenType::Less, 1, "<")
            ]
        ),
        (
            less_equal,
            "<=",
            vec![Token::new(TokenType::LessEqual, 1, "<=")]
        ),
        (greater, ">", vec![Token::new(TokenType::Greater, 1, ">")]),
        (
            greater_greater,
            ">>",
            vec![
                Token::new(TokenType::Greater, 1, ">"),
                Token::new(TokenType::Greater, 1, ">")
            ]
        ),
        (
            greater_equal,
            ">=",
            vec![Token::new(TokenType::GreaterEqual, 1, ">=")]
        ),
        (
            whitespace_1,
            " = ",
            vec![Token::new(TokenType::Equal, 1, "=")]
        ),
        (
            whitespace_2,
            "\n = ",
            vec![Token::new(TokenType::Equal, 2, "=")]
        ),
        (
            whitespace_3,
            "\t\n = ",
            vec![Token::new(TokenType::Equal, 2, "=")]
        ),
        (
            comment_1,
            "// Hello, World\n=",
            vec![Token::new(TokenType::Equal, 2, "=")]
        ),
        (
            comment_2,
            "// Hello,\n // World\n =",
            vec![Token::new(TokenType::Equal, 3, "=")]
        ),
        (
            comment_3,
            "=// Hello World",
            vec![Token::new(TokenType::Equal, 1, "=")]
        ),
        (
            comment_4,
            "/ /",
            vec![
                Token::new(TokenType::Slash, 1, "/"),
                Token::new(TokenType::Slash, 1, "/")
            ]
        ),
        (
            number_1,
            "123=",
            vec![
                Token::new(TokenType::Number, 1, "123"),
                Token::new(TokenType::Equal, 1, "="),
            ]
        ),
        (
            number_2,
            "123.4=",
            vec![
                Token::new(TokenType::Number, 1, "123.4"),
                Token::new(TokenType::Equal, 1, "="),
            ]
        ),
        (
            string_1,
            "\"123\"4",
            vec![
                Token::new(TokenType::String, 1, "\"123\""),
                Token::new(TokenType::Number, 1, "4"),
            ]
        ),
        (
            string_2,
            "\"höla\"",
            vec![Token::new(TokenType::String, 1, "\"höla\""),]
        ),
        (
            identifier_1,
            "mint_mundo_1",
            vec![Token::new(TokenType::Identifier, 1, "mint_mundo_1")]
        ),
        (
            keyword_and,
            "and",
            vec![Token::new(TokenType::And, 1, "and")]
        ),
        (
            keyword_class,
            "class",
            vec![Token::new(TokenType::Class, 1, "class")]
        ),
        (
            keyword_else,
            "else",
            vec![Token::new(TokenType::Else, 1, "else")]
        ),
        (
            keyword_false,
            "false",
            vec![Token::new(TokenType::False, 1, "false")]
        ),
        (
            keyword_for,
            "for",
            vec![Token::new(TokenType::For, 1, "for")]
        ),
        (
            keyword_fun,
            "fun",
            vec![Token::new(TokenType::Fun, 1, "fun")]
        ),
        (keyword_if, "if", vec![Token::new(TokenType::If, 1, "if")]),
        (
            keyword_nil,
            "nil",
            vec![Token::new(TokenType::Nil, 1, "nil")]
        ),
        (keyword_or, "or", vec![Token::new(TokenType::Or, 1, "or")]),
        (
            keyword_print,
            "print",
            vec![Token::new(TokenType::Print, 1, "print")]
        ),
        (
            keyword_return,
            "return",
            vec![Token::new(TokenType::Return, 1, "return")]
        ),
        (
            keyword_super,
            "super",
            vec![Token::new(TokenType::Super, 1, "super")]
        ),
        (
            keyword_this,
            "this",
            vec![Token::new(TokenType::This, 1, "this")]
        ),
        (
            keyword_true,
            "true",
            vec![Token::new(TokenType::True, 1, "true")]
        ),
        (
            keyword_var,
            "var",
            vec![Token::new(TokenType::Var, 1, "var")]
        ),
        (
            keyword_while,
            "while",
            vec![Token::new(TokenType::While, 1, "while")]
        ),
        (
            line_number_1,
            "\nfalse",
            vec![Token::new(TokenType::False, 2, "false")]
        ),
        (
            line_number_2,
            "// comment\nfalse",
            vec![Token::new(TokenType::False, 2, "false")]
        ),
    );
}
