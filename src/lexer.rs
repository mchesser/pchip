use std::char::is_whitespace;
use error::InputPos;

#[deriving(PartialEq, Clone, Show)]
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

#[deriving(Show)]
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
        let (val, rest) = self.remaining.slice_shift_char();
        match val {
            // Move to the next line
            Some('\n') => {
                self.pos.line += 1;
                self.pos.col = 0;
            },

            // Discard carrage return characters
            Some('\r') => {},

            // For other characters increase the column position
            Some(_) => self.pos.col += 1,

            None => {},
        }
        self.remaining = rest;
    }

    fn read_white_space_or_comment(&mut self) {
        while self.remaining.len() > 0 {
            match self.remaining.char_at(0) {
                // Match whitespace
                c if is_whitespace(c) => self.bump(),

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

impl<'a> Iterator<Token> for Lexer<'a> {
    fn next(&mut self) -> Option<Token> {
        // Read up to the next proper token
        self.read_white_space_or_comment();

        let len = self.remaining.len();
        if len == 0 {
            return None;
        }

        let mut token_len = 1;
        let token_val = match self.remaining.char_at(0) {
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
                if len == 1 { Assignment }
                else {
                    match self.remaining.char_at(1) {
                        '=' => { token_len += 1; Equal },
                        _ => Assignment
                    }
                }
            },

            '+' => {
                if len == 1 { Plus }
                else {
                    match self.remaining.char_at(1) {
                        '=' => { token_len += 1; PlusEq },
                        _ => Plus
                    }
                }
            },

            '-' => {
                if len == 1 { Minus }
                else {
                    match self.remaining.char_at(1) {
                        '=' => { token_len += 1; MinusEq },
                        '>' => { token_len += 1; RightArrow },
                        _ => Minus
                    }
                }
            },

            '0'...'9' => {
                token_len = scan_token(self.remaining);
                let number_str = self.remaining.slice_to(token_len);
                match from_str(number_str) {
                    Some(n) => LitNum(n),
                    None => panic!("Invalid number"),
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
                let result = LitString(self.remaining.slice_to(token_len).into_string());
                token_len += 1;
                result
            },

            '\'' => {
                if len < 3 {
                    panic!("Invalid char literal");
                }
                self.bump();
                let result = LitChar(self.remaining.char_at(0));
                self.bump();
                if self.remaining.char_at(0) != '\'' {
                    panic!("Unclosed '");
                }
                result
            },

            _ => {
                token_len = scan_token(self.remaining);
                let token_str = self.remaining.slice_to(token_len);
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
            pos: self.pos.clone(),
        };
        self.pos.col += token_len;
        self.remaining = self.remaining.slice_from(token_len);
        Some(token)
    }
}

/// Scans till the end of the token returning the index of the end of the token
fn scan_token(string: &str) -> uint {
    const TOKEN_BOUNDS: &'static [char] = &[
        ' ', '\t', '\n', '\r', '#', ':', ';', ',', '(', ')', '{', '}', '[', ']', '.', '*', '&', '=',
        '+', '-', '"', '\'',
    ];

    match string.find(TOKEN_BOUNDS) {
        Some(n) => n,
        None => string.len()
    }
}
