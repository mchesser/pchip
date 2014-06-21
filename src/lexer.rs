///
/// Description: Parse the source code into tokens
///

#[deriving(PartialEq, Clone, Show)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Assignment,
    Comma,
    LitNum(u16),
    Let,
    If,
    For,
    While,
    Loop,
    Break,
    Else,
    Fn,
    Ident(String),
    Equal,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    StatementEnd,
    Eof,
}

pub struct Lexer<'a> {
    remaining: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            remaining: source.trim()
        }
    }
}

impl<'a> Iterator<Token> for Lexer<'a> {
    fn next(&mut self) -> Option<Token> {
        let len = self.remaining.len();
        if len == 0 {
            return None;
        }

        let mut token_end = 1;
        let token = match self.remaining.char_at(0) {
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            ';' => StatementEnd,
            ',' => Comma,
            '=' => {
                if len == 1 { Assignment }
                else {
                    match self.remaining.char_at(1) {
                        '=' => { token_end += 1; Equal },
                        _   => Assignment
                    }
                }
            },
            '+' => {
                if len == 1 { Plus }
                else {
                    match self.remaining.char_at(1) {
                        '=' => { token_end += 1; PlusEq },
                        _   => Plus
                    }
                }
            },
            '-' => {
                if len == 1 { Minus }
                else {
                    match self.remaining.char_at(1) {
                        '=' => { token_end += 1; MinusEq },
                        _   => Minus
                    }
                }
            },
            '#' => {
                token_end = 0;
                let iter = self.remaining.splitn('\n', 1);
                match iter.skip(1).next() {
                    Some(str) => {
                        self.remaining = str.trim_left();
                        match self.next() {
                            Some(s) => s,
                            None => Eof,
                        }
                    },
                    None => Eof
                }
            },
            '0'..'9' => {
                token_end = scan_token(self.remaining);
                match from_str(self.remaining.slice_to(token_end)) {
                    Some(n) => LitNum(n),
                    None    => fail!("Invalid number")
                }
            },
            _ => {
                token_end = scan_token(self.remaining);
                let token_str = self.remaining.slice_to(token_end);
                match token_str {
                    "let"   => Let,
                    "if"    => If,
                    "for"   => For,
                    "while" => While,
                    "loop"  => Loop,
                    "break" => Break,
                    "else"  => Else,
                    "fn"    => Fn,
                    "true"  => LitNum(1),
                    "false" => LitNum(0),
                    _       => Ident(token_str.to_string())
                }
            }
        };
        self.remaining = self.remaining.slice_from(token_end).trim_left();
        Some(token)
    }
}

/// Scans till the end of the token returning the index of the end of the token
fn scan_token(string: &str) -> uint {
    static TOKEN_BOUNDS: &'static [char] = &[
        ' ', '\t', '\n', '#', ';', ',', '(', ')', '{', '}', '.', '=', '+', '-'
    ];
    match string.find(TOKEN_BOUNDS) {
        Some(n) => n,
        None    => string.len()
    }
}
