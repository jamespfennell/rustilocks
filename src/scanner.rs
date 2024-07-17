use crate::error::ScannerError;

pub struct Scanner<'a> {
    source: &'a str,
    cache: Option<Token<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Scanner<'a> {
        Scanner {
            source: src,
            cache: None,
        }
    }

    pub fn next(&mut self) -> Result<Option<Token<'a>>, Box<ScannerError<'a>>> {
        match self.cache.take() {
            None => self.scan(),
            Some(token) => Ok(Some(token)),
        }
    }

    pub fn peek(&mut self) -> Result<Option<Token<'a>>, Box<ScannerError<'a>>> {
        if self.cache.is_none() {
            self.cache = self.scan()?;
        }
        Ok(self.cache)
    }

    pub fn consume(&mut self) -> Result<(), Box<ScannerError<'a>>> {
        self.next()?;
        Ok(())
    }

    fn scan(&mut self) -> Result<Option<Token<'a>>, Box<ScannerError<'a>>> {
        self.source = skip_whitespace(self.source);
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
                            return Err(Box::new(ScannerError::UnterminatedString(self.source)))
                        }
                        Some('"') => break,
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
            _ => return Err(Box::new(ScannerError::InvalidCharacter(self.source))),
        };
        let token_text = &self.source[..len];
        self.source = &self.source[len..];
        Ok(Some(Token::new(token_type, token_text)))
    }
}

fn skip_whitespace(source: &str) -> &str {
    let mut chars = source.chars();
    let mut bytes_to_skip = 0;
    while let Some(c) = chars.next() {
        match c {
            ' ' | '\r' | '\t' | '\n' => {
                bytes_to_skip += 1;
            }
            '/' => {
                if chars.next() == Some('/') {
                    bytes_to_skip += 2;
                    for c in chars.by_ref() {
                        bytes_to_skip += c.len_utf8();
                        if c == '\n' {
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
    &source[bytes_to_skip..]
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
    pub source: &'a str,
}

impl<'a> Token<'a> {
    fn new(token_type: TokenType, source: &'a str) -> Token<'a> {
        Token { token_type, source }
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
                while let Some(token) = scanner.scan().expect("valid token") {
                    got.push(token);
                }
                assert_eq!($output, got);
            }
      )*
        };
    }

    scanner_tests!(
        (left_paren, "(", vec![Token::new(TokenType::LeftParen, "(")]),
        (
            right_paren,
            ")",
            vec![Token::new(TokenType::RightParen, ")")]
        ),
        (left_brace, "{", vec![Token::new(TokenType::LeftBrace, "{")]),
        (
            right_brace,
            "}",
            vec![Token::new(TokenType::RightBrace, "}")]
        ),
        (semicolon, ";", vec![Token::new(TokenType::Semicolon, ";")]),
        (comma, ",", vec![Token::new(TokenType::Comma, ",")]),
        (dot, ".", vec![Token::new(TokenType::Dot, ".")]),
        (minus, "-", vec![Token::new(TokenType::Minus, "-")]),
        (plus, "+", vec![Token::new(TokenType::Plus, "+")]),
        (slash, "/", vec![Token::new(TokenType::Slash, "/")]),
        (star, "*", vec![Token::new(TokenType::Star, "*")]),
        (bang, "!", vec![Token::new(TokenType::Bang, "!")]),
        (
            bang_bang,
            "!!",
            vec![
                Token::new(TokenType::Bang, "!"),
                Token::new(TokenType::Bang, "!")
            ]
        ),
        (
            bang_equal,
            "!=",
            vec![Token::new(TokenType::BangEqual, "!=")]
        ),
        (equal, "=", vec![Token::new(TokenType::Equal, "=")]),
        (
            equal_bang,
            "=!",
            vec![
                Token::new(TokenType::Equal, "="),
                Token::new(TokenType::Bang, "!")
            ]
        ),
        (
            equal_equal,
            "==",
            vec![Token::new(TokenType::EqualEqual, "==")]
        ),
        (less, "<", vec![Token::new(TokenType::Less, "<")]),
        (
            less_less,
            "<<",
            vec![
                Token::new(TokenType::Less, "<"),
                Token::new(TokenType::Less, "<")
            ]
        ),
        (
            less_equal,
            "<=",
            vec![Token::new(TokenType::LessEqual, "<=")]
        ),
        (greater, ">", vec![Token::new(TokenType::Greater, ">")]),
        (
            greater_greater,
            ">>",
            vec![
                Token::new(TokenType::Greater, ">"),
                Token::new(TokenType::Greater, ">")
            ]
        ),
        (
            greater_equal,
            ">=",
            vec![Token::new(TokenType::GreaterEqual, ">=")]
        ),
        (whilespace_1, " = ", vec![Token::new(TokenType::Equal, "=")]),
        (
            whilespace_2,
            "\n = ",
            vec![Token::new(TokenType::Equal, "=")]
        ),
        (
            whilespace_3,
            "\t\n = ",
            vec![Token::new(TokenType::Equal, "=")]
        ),
        (
            comment_1,
            "// Hello, World\n=",
            vec![Token::new(TokenType::Equal, "=")]
        ),
        (
            comment_2,
            "// Hello,\n // World\n =",
            vec![Token::new(TokenType::Equal, "=")]
        ),
        (
            comment_3,
            "=// Hello World",
            vec![Token::new(TokenType::Equal, "=")]
        ),
        (
            comment_4,
            "/ /",
            vec![
                Token::new(TokenType::Slash, "/"),
                Token::new(TokenType::Slash, "/")
            ]
        ),
        (
            number_1,
            "123=",
            vec![
                Token::new(TokenType::Number, "123"),
                Token::new(TokenType::Equal, "="),
            ]
        ),
        (
            number_2,
            "123.4=",
            vec![
                Token::new(TokenType::Number, "123.4"),
                Token::new(TokenType::Equal, "="),
            ]
        ),
        (
            string_1,
            "\"123\"4",
            vec![
                Token::new(TokenType::String, "\"123\""),
                Token::new(TokenType::Number, "4"),
            ]
        ),
        (
            string_2,
            "\"höla\"",
            vec![Token::new(TokenType::String, "\"höla\""),]
        ),
        (
            identifier_1,
            "mint_mundo_1",
            vec![Token::new(TokenType::Identifier, "mint_mundo_1")]
        ),
        (keyword_and, "and", vec![Token::new(TokenType::And, "and")]),
        (
            keyword_class,
            "class",
            vec![Token::new(TokenType::Class, "class")]
        ),
        (
            keyword_else,
            "else",
            vec![Token::new(TokenType::Else, "else")]
        ),
        (
            keyword_false,
            "false",
            vec![Token::new(TokenType::False, "false")]
        ),
        (keyword_for, "for", vec![Token::new(TokenType::For, "for")]),
        (keyword_fun, "fun", vec![Token::new(TokenType::Fun, "fun")]),
        (keyword_if, "if", vec![Token::new(TokenType::If, "if")]),
        (keyword_nil, "nil", vec![Token::new(TokenType::Nil, "nil")]),
        (keyword_or, "or", vec![Token::new(TokenType::Or, "or")]),
        (
            keyword_print,
            "print",
            vec![Token::new(TokenType::Print, "print")]
        ),
        (
            keyword_return,
            "return",
            vec![Token::new(TokenType::Return, "return")]
        ),
        (
            keyword_super,
            "super",
            vec![Token::new(TokenType::Super, "super")]
        ),
        (
            keyword_this,
            "this",
            vec![Token::new(TokenType::This, "this")]
        ),
        (
            keyword_true,
            "true",
            vec![Token::new(TokenType::True, "true")]
        ),
        (keyword_var, "var", vec![Token::new(TokenType::Var, "var")]),
        (
            keyword_while,
            "while",
            vec![Token::new(TokenType::While, "while")]
        ),
    );
}
