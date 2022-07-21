pub struct Scanner<'a> {
    pub source: &'a str,
}

impl<'a> Scanner<'a> {
    pub fn scan(&mut self) -> Result<Option<Token<'a>>, ScannerError<'a>> {
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
                        None => return Err(ScannerError::UnterminatedString(self.source)),
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
                            while let Some(c) = chars.next() {
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
                while let Some(c) = chars.next() {
                    match c {
                        'A'..='Z' | 'a'..='z' | '_' | '0'..='9' => {
                            len += 1;
                        }
                        _ => break,
                    }
                }
                (identifier_token_type(&self.source[..len]), len)
            }
            _ => return Err(ScannerError::InvalidCharacter(self.source)),
        };
        let token_text = &self.source[..len];
        self.source = &self.source[len..];
        Ok(Some(Token::new(token_type, token_text)))
    }

    fn skip_whitespace(&mut self) {
        let mut chars = self.source.chars();
        let mut bytes_to_skip = 0;
        while let Some(c) = chars.next() {
            match c {
                ' ' | '\r' | '\t' | '\n' => {
                    bytes_to_skip += 1;
                }
                '/' => {
                    if chars.next() == Some('/') {
                        bytes_to_skip += 2;
                        while let Some(c) = chars.next() {
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

#[derive(Debug)]
pub enum ScannerError<'a> {
    InvalidCharacter(&'a str),
    UnterminatedString(&'a str),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub source: &'a str,
}

impl<'a> Token<'a> {
    fn new(token_type: TokenType, source: &'a str) -> Token<'a> {
        Token { token_type, source }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! scanner_test {
        ($name: ident, $input: expr, $output: expr) => {
            #[test]
            fn $name() {
                let mut scanner = Scanner { source: $input };
                let mut got = vec![];
                while let Some(token) = scanner.scan().unwrap() {
                    got.push(token);
                }
                assert_eq!($output, got);
            }
        };
    }

    scanner_test!(left_paren, "(", vec![Token::new(TokenType::LeftParen, "(")]);
    scanner_test!(
        right_paren,
        ")",
        vec![Token::new(TokenType::RightParen, ")")]
    );
    scanner_test!(left_brace, "{", vec![Token::new(TokenType::LeftBrace, "{")]);
    scanner_test!(
        right_brace,
        "}",
        vec![Token::new(TokenType::RightBrace, "}")]
    );
    scanner_test!(semicolon, ";", vec![Token::new(TokenType::Semicolon, ";")]);
    scanner_test!(comma, ",", vec![Token::new(TokenType::Comma, ",")]);
    scanner_test!(dot, ".", vec![Token::new(TokenType::Dot, ".")]);
    scanner_test!(minus, "-", vec![Token::new(TokenType::Minus, "-")]);
    scanner_test!(plus, "+", vec![Token::new(TokenType::Plus, "+")]);
    scanner_test!(slash, "/", vec![Token::new(TokenType::Slash, "/")]);
    scanner_test!(star, "*", vec![Token::new(TokenType::Star, "*")]);
    scanner_test!(bang, "!", vec![Token::new(TokenType::Bang, "!")]);
    scanner_test!(
        bang_bang,
        "!!",
        vec![
            Token::new(TokenType::Bang, "!"),
            Token::new(TokenType::Bang, "!")
        ]
    );
    scanner_test!(
        bang_equal,
        "!=",
        vec![Token::new(TokenType::BangEqual, "!=")]
    );
    scanner_test!(equal, "=", vec![Token::new(TokenType::Equal, "=")]);
    scanner_test!(
        equal_bang,
        "=!",
        vec![
            Token::new(TokenType::Equal, "="),
            Token::new(TokenType::Bang, "!")
        ]
    );
    scanner_test!(
        equal_equal,
        "==",
        vec![Token::new(TokenType::EqualEqual, "==")]
    );
    scanner_test!(less, "<", vec![Token::new(TokenType::Less, "<")]);
    scanner_test!(
        less_less,
        "<<",
        vec![
            Token::new(TokenType::Less, "<"),
            Token::new(TokenType::Less, "<")
        ]
    );
    scanner_test!(
        less_equal,
        "<=",
        vec![Token::new(TokenType::LessEqual, "<=")]
    );
    scanner_test!(greater, ">", vec![Token::new(TokenType::Greater, ">")]);
    scanner_test!(
        greater_greater,
        ">>",
        vec![
            Token::new(TokenType::Greater, ">"),
            Token::new(TokenType::Greater, ">")
        ]
    );
    scanner_test!(
        greater_equal,
        ">=",
        vec![Token::new(TokenType::GreaterEqual, ">=")]
    );

    scanner_test!(whilespace_1, " = ", vec![Token::new(TokenType::Equal, "=")]);
    scanner_test!(
        whilespace_2,
        "\n = ",
        vec![Token::new(TokenType::Equal, "=")]
    );
    scanner_test!(
        whilespace_3,
        "\t\n = ",
        vec![Token::new(TokenType::Equal, "=")]
    );

    scanner_test!(
        comment_1,
        "// Hello, World\n=",
        vec![Token::new(TokenType::Equal, "=")]
    );
    scanner_test!(
        comment_2,
        "// Hello,\n // World\n =",
        vec![Token::new(TokenType::Equal, "=")]
    );
    scanner_test!(
        comment_3,
        "=// Hello World",
        vec![Token::new(TokenType::Equal, "=")]
    );
    scanner_test!(
        comment_4,
        "/ /",
        vec![
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Slash, "/")
        ]
    );
    scanner_test!(
        number_1,
        "123=",
        vec![
            Token::new(TokenType::Number, "123"),
            Token::new(TokenType::Equal, "="),
        ]
    );
    scanner_test!(
        number_2,
        "123.4=",
        vec![
            Token::new(TokenType::Number, "123.4"),
            Token::new(TokenType::Equal, "="),
        ]
    );
    scanner_test!(
        string_1,
        "\"123\"4",
        vec![
            Token::new(TokenType::String, "\"123\""),
            Token::new(TokenType::Number, "4"),
        ]
    );
    scanner_test!(
        string_2,
        "\"höla\"",
        vec![Token::new(TokenType::String, "\"höla\""),]
    );
    scanner_test!(
        identifier_1,
        "mint_mundo_1",
        vec![Token::new(TokenType::Identifier, "mint_mundo_1")]
    );
    scanner_test!(keyword_and, "and", vec![Token::new(TokenType::And, "and")]);
    scanner_test!(
        keyword_class,
        "class",
        vec![Token::new(TokenType::Class, "class")]
    );
    scanner_test!(
        keyword_else,
        "else",
        vec![Token::new(TokenType::Else, "else")]
    );
    scanner_test!(
        keyword_false,
        "false",
        vec![Token::new(TokenType::False, "false")]
    );
    scanner_test!(keyword_for, "for", vec![Token::new(TokenType::For, "for")]);
    scanner_test!(keyword_fun, "fun", vec![Token::new(TokenType::Fun, "fun")]);
    scanner_test!(keyword_if, "if", vec![Token::new(TokenType::If, "if")]);
    scanner_test!(keyword_nil, "nil", vec![Token::new(TokenType::Nil, "nil")]);
    scanner_test!(keyword_or, "or", vec![Token::new(TokenType::Or, "or")]);
    scanner_test!(
        keyword_print,
        "print",
        vec![Token::new(TokenType::Print, "print")]
    );
    scanner_test!(
        keyword_return,
        "return",
        vec![Token::new(TokenType::Return, "return")]
    );
    scanner_test!(
        keyword_super,
        "super",
        vec![Token::new(TokenType::Super, "super")]
    );
    scanner_test!(
        keyword_this,
        "this",
        vec![Token::new(TokenType::This, "this")]
    );
    scanner_test!(
        keyword_true,
        "true",
        vec![Token::new(TokenType::True, "true")]
    );
    scanner_test!(keyword_var, "var", vec![Token::new(TokenType::Var, "var")]);
    scanner_test!(
        keyword_while,
        "while",
        vec![Token::new(TokenType::While, "while")]
    );
}
