use error::InputPos;
pub use lexer::TokenValue::*;

#[derive(PartialEq, Clone, Debug)]
pub enum TokenValue {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Let,
    Const,
    Assignment,
    RightArrow,

    Comma,
    Colon,
    SemiColon,
    Dot,
    Star,
    Amp,

    Equal,
    Plus,
    PlusEq,
    Minus,
    MinusEq,

    Eof,

    For,
    Range,
    In,
    While,
    Loop,
    Break,
    Return,

    If,
    Else,
    Struct,
    As,
    Fn,
    Asm,

    Bool,
    Char,
    Int,
    Uint,
    Any,

    True,
    False,
    LitNum(i32),
    LitChar(char),
    LitString(String),
    Null,
    Ident(String),
}

#[derive(Debug)]
pub struct Token {
    pub value: TokenValue,
    pub pos: InputPos,
}

pub struct Lexer<'a> {
    remaining: &'a str,
    pos: InputPos,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            remaining: source,
            pos: InputPos::start(),
        }
    }

    fn bump(&mut self) {
        if let Some(next) = self.remaining.chars().next() {
            match next {
                // Move to the next line
                '\n' => {
                    self.pos.line += 1;
                    self.pos.col = 0;
                },

                // Discard carrage return characters
                '\r' => {},

                // For other characters increase the column position
                _ => self.pos.col += 1,
            }
            self.remaining = &self.remaining[next.len_utf8()..];
        }
    }

    fn read_white_space_or_comment(&mut self) {
        while let Some(next) = self.remaining.chars().next() {
            match next {
                // Match whitespace
                c if c.is_whitespace() => self.bump(),

                // Match comments
                '#' => {
                    let comment_line = self.pos.line;
                    while self.pos.line == comment_line && self.remaining.len() > 0 {
                        self.bump();
                    }
                },

                // Match other chars
                _ => break,
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        // Read up to the next proper token
        self.read_white_space_or_comment();

        let len = self.remaining.len();
        if len == 0 {
            return None;
        }

        let mut token_len = 1;
        let token_val = match self.remaining.chars().next().unwrap() {
            '(' => LeftParen,
            ')' => RightParen,

            '{' => LeftBrace,
            '}' => RightBrace,

            '[' => LeftBracket,
            ']' => RightBracket,

            ';' => SemiColon,
            ':' => Colon,
            ',' => Comma,
            '.' => Dot,
            '*' => Star,
            '&' => Amp,

            '=' => {
                match self.remaining.chars().nth(1) {
                    Some('=') => {
                        token_len += 1;
                        Equal
                    },
                    _ => Assignment,
                }
            },

            '+' => {
                match self.remaining.chars().nth(1) {
                    Some('=') => {
                        token_len += 1;
                        PlusEq
                    },
                    _ => Plus,
                }
            },

            '-' => {
                match self.remaining.chars().nth(1) {
                    Some('=') => {
                        token_len += 1;
                        MinusEq
                    },
                    Some('>') => {
                        token_len += 1;
                        RightArrow
                    },
                    _ => Minus,
                }
            },

            '0'...'9' => {
                token_len = scan_token(self.remaining);
                let number_str = &self.remaining[..token_len];
                match number_str.parse() {
                    Ok(n) => LitNum(n),
                    Err(e) => panic!("ICE: Invalid number: {}", e),
                }
            },

            '"' => {
                self.bump();
                token_len = match self.remaining.find('"') {
                    Some(offset) => offset,
                    None => {
                        panic!("Unclosed \" ");
                    },
                };
                let result = LitString(self.remaining[..token_len].to_string());
                token_len += 1;
                result
            },

            '\'' => {
                if len < 3 {
                    panic!("Invalid char literal");
                }
                self.bump();
                let result = LitChar(self.remaining.chars().next().unwrap());
                self.bump();
                if self.remaining.chars().next().unwrap() != '\'' {
                    panic!("Unclosed '");
                }
                result
            },

            _ => {
                token_len = scan_token(self.remaining);
                let token_str = &self.remaining[..token_len];
                match token_str {
                    "let"    => Let,
                    "const"  => Const,
                    "if"     => If,
                    "for"    => For,
                    "range"  => Range,
                    "in"     => In,
                    "while"  => While,
                    "loop"   => Loop,
                    "break"  => Break,
                    "return" => Return,
                    "else"   => Else,
                    "asm"    => Asm,
                    "fn"     => Fn,
                    "struct" => Struct,
                    "as"     => As,
                    "true"   => True,
                    "false"  => False,
                    "null"   => Null,
                    "int"    => Int,
                    "char"   => Char,
                    "bool"   => Bool,
                    "any"    => Any,
                    _        => Ident(token_str.to_string())
                }
            }
        };

        let token = Token {
            value: token_val,
            pos: self.pos,
        };
        self.pos.col += token_len;
        self.remaining = &self.remaining[token_len..];
        Some(token)
    }
}

/// Scans till the end of the token returning the index of the end of the token
fn scan_token(string: &str) -> usize {
    const TOKEN_BOUNDS: &'static [char] = &[
        ' ', '\t', '\n', '\r', '#', ':', ';', ',', '(', ')', '{', '}', '[', ']', '.', '*', '&', '=',
        '+', '-', '"', '\'',
    ];

    match string.find(TOKEN_BOUNDS) {
        Some(n) => n,
        None => string.len()
    }
}
