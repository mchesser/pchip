///
/// Description: Translate from pchip to assembly
///

use std::hashmap::HashMap;

mod lexer;
mod asm;

enum Operation {
    RawOp(asm::Operation),
    UnknownAddr(asm::Operation),
    Marker(Option<asm::Operation>)
}


struct Variable {
    offset: u16
}
 

pub struct Parser<'a> {
    source: ~[lexer::Token],
    variables: HashMap<&'a str, Variable>,
    last_offset: u16,
    pos: uint,
}

impl<'a> Parser<'a> {
    pub fn evaluate(source_code: &'a str) -> ~[Operation] {
        let mut parser = Parser {
            source: lexer::Lexer::new(source_code).collect(),
            variables: HashMap::new(),
            last_offset: 0,
            pos: 0
        };
        
        let mut operations = ~[];
        while parser.source[parser.pos] != lexer::Eof {
            operations.push_all_move(parser.parse());
        }
        
        operations
    }
    
    fn parse(&mut self) -> ~[Operation] {
        match self.source[self.pos] {
            lexer::Let => self.parse_let(),
            lexer::NonKeyWord(s) => self.parse_assignment(s),
            _ => fail!("ICE")
        }
    }

    fn parse_let(&mut self) -> ~[Operation] {
        let var_name = match self.source[self.pos] {
            lexer::NonKeyWord(s) => s.as_slice(),
            _ => fail!("Error: expected `NonKeyWord` but found `{:?}`", self.source[self.pos])
        };
        
        if self.variables.contains_key(&var_name) {
            fail!("Error: variable shadowing unsupported. (`{}` was already defined)", var_name);
        } 
        
        self.last_offset += 1;
        self.variables.insert(var_name, Variable { offset: self.last_offset });
        
        self.parse_assignment(var_name)
    }
    
    fn parse_assignment(&mut self, var_name: &'a str) -> ~[Operation] {
        if !self.variables.contains_key(&var_name) {
            fail!("Variable {} is undefined", var_name)
        }
        self.pos += 1;
        match self.source[self.pos] {
            lexer::Assignment => {},
            _  => fail!("Error: expected `=` but found `{:?}` (FIXME: should be able to define variables before use)", self.source[self.pos])
        }
        self.pos += 1;
        let mut operations = self.parse_expression();
        operations.push(UnknownAddr(asm::SetAddress(asm::Addr(self.variables.get(&var_name).offset))));
        operations.push(RawOp(asm::Write(asm::RegId(1))));
        
        operations
    }
    
    fn parse_expression(&mut self) -> ~[Operation] {
        let mut operations = match self.source[self.pos] {
            lexer::NonKeyWord(s) => self.load_variable(s),
            lexer::Num(n)        => ~[RawOp(asm::SetV(asm::RegId(0), n as u8))],
            lexer::Plus          => self.parse_operator(lexer::Plus),
            lexer::Minus         => self.parse_operator(lexer::Minus),
            _ => fail!("Error: unexpected `{:?}` in EXPRESSION", self.source[self.pos]),
        };
        
        match self.source[self.pos] {
            //StatementEnd => {},
            _ => operations.push_all_move(self.parse_expression())
        }
        
        operations
    }
    
    fn load_variable(&mut self, var_name: &'a str) -> ~[Operation] {
        self.pos += 1;
        // Match system operations here too!
        ~[UnknownAddr(asm::SetAddress(asm::Addr(self.variables.get(&var_name).offset))),
                RawOp(asm::Read(asm::RegId(1)))]
    }
    
    fn parse_operator(&mut self, op: lexer::Token) -> ~[Operation] {
        if self.pos == 0 {
            fail!("Invalid operator position");
        }
        
        let operations = match (self.source[self.pos-1], self.source[self.pos+1]) {
            (lexer::NonKeyWord(_), lexer::NonKeyWord(s)) => {
                let mut ops = ~[RawOp(asm::Set(asm::RegId(1), asm::RegId(0)))];
                ops.push_all_move(self.load_variable(s));
                ops
            },
            (lexer::Num(_), lexer::NonKeyWord(s)) => {
                let mut ops = ~[RawOp(asm::Set(asm::RegId(1), asm::RegId(0)))];
                ops.push_all_move(self.load_variable(s));
                ops
            },
            (lexer::Num(_), lexer::Num(n)) => {
                ~[RawOp(asm::Set(asm::RegId(1), asm::RegId(0))), RawOp(asm::SetV(asm::RegId(0), n as u8))]
            },
            _ => fail!("Error: invalid operands")
        };
        
        match op {
            lexer::Plus  => operations.push(RawOp(asm::Add(asm::RegId(0), asm::RegId(1)))),
            lexer::Minus => operations.push(RawOp(asm::Sub2(asm::RegId(0), asm::RegId(1)))),
            _ => fail!("ICE: invalid use of parse_operator")
        }
        
        self.pos += 2;
        
        operations
    }
} 

/*
system keywords:
    __clear      -- Clear
    __draw       -- Draw(Num, Num, Num)
    __random     -- Random(Num, Num)
    __set_delay  -- SetDelay(Num)
    __get_delay  -- GetDelay(Num)
    __set_sound  -- SetSound(Num)
    __get_font   -- GetFont(Num)
    __key_wait   -- KeyWait(Num)
}


Sample program 1

let a = 10  -> [SetV(0, 10), SetAddress(i  ), Write(1)]
let b = 50  -> [SetV(0, 50), SetAddress(i+1), Write(1)]
let c = 0   -> [SetV(0, 0),  SetAddress(i+2), Write(1)]

if a == b  -> [SetAddress(i), Read(1), Set(1, 0), SetAddress(i+1), Read(1), Compare(1, 0)]
    c = 3  -> [SetV(0, 3), SetAddress(i+2), Write(1)]
    
    
Sample program 2

let x = 0;                          // [SetV(0, 10), SetAddress(1), Write(1)]
draw(x, 0, 5, __get_font(1))        // [SetAddress(i), Read(1), Set(1, 0), SetV(2, 5), GetFont(1), Draw(1, 2, 5)]
loop {                              // 
    if x == 10 {                    // [SetAddress(i), Read(1), CompareNotV(0, 10)]
        break;                      // [Jump(Loop)]
    }                               //
    draw(x, 0, 5, __get_font(1));   // [SetAddress(i), Read(1), Set(1, 0), SetV(2, 5), GetFont(1), Draw(1, 2, 5)]
    x += 1;                         // [SetAddress(i), Read(1), AddV(0, 1), Write(1)]
    draw(x, 0, 5, __get_font(1));   // [SetAddress(i), Read(1), Set(1, 0), SetV(2, 5), GetFont(1), Draw(1, 2, 5)]
}

__key_wait(0);                      // KeyWait(0)


*/
