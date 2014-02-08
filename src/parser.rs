///
/// Description: Parse soruce code, returning a vector of bytes representing the compiled program
///

use std::hashmap::HashMap;
use parser::trans::MarkerId;
use parser::trans::VarId;

mod lexer;
mod trans;
mod asm;

#[deriving(Eq)]
enum BlockType {
    IfBlock,
    LoopBlock,
    FunctionBlock,
}

struct MarkerStack {
    priv markers: ~[(BlockType, MarkerId)],
    last_marker: MarkerId
}

impl MarkerStack {
    fn new() -> MarkerStack {
        MarkerStack {
            markers: ~[],
            last_marker: 0
        }
    }

    /// Add a new marker to the marker stack
    fn push(&mut self, block_type: BlockType) {
        self.markers.push((block_type, self.last_marker));
        self.last_marker += 1;
    }

    /// Pop the last marker added
    fn pop(&mut self) -> (BlockType, MarkerId) {
        self.markers.pop()
    }

    /// Keep poping markers until a marker of the correct type is found
    fn pop_until(&mut self, block_type: BlockType) -> (BlockType, MarkerId) {
        loop {
            let (btype, id) = self.pop();
            if btype == block_type {
                return (btype, id);
            }
        }
    }

    /// Get the last marker added
    fn top(&self) -> (BlockType, MarkerId) {
        *self.markers.last()
    }
}

static SYSTEM_CALLS: [&'static str, ..8] = [
    "__clear", "__draw_pos", "__set_addr", "__draw", "__random", "__set_delay",
    "__get_font", "__key_wait"
];

struct Parser {
    variables: HashMap<~str, VarId>,
    var_index: VarId,
    marker_stack: MarkerStack,
    code: ~[lexer::Token],
    pos: uint,
}

pub fn parse(source_code: &str) -> ~[u8] {
    let mut parser = Parser {
        variables: HashMap::new(),
        var_index: 0,
        marker_stack: MarkerStack::new(),
        code: lexer::Lexer::new(source_code).collect(),
        pos: 0,
    };
    println!("{:?}\n", parser.code);
    let ops = trans::block_to_asm(parser.parse_block(FunctionBlock));
    for op in ops.iter() {
        println!("{:?}", op);
    }
    println!("");
    asm::compile(ops, parser.marker_stack.last_marker)
}

impl Parser {
    fn parse_block(&mut self, block_type: BlockType) -> trans::Block {
        let mut statements = ~[];
        self.marker_stack.push(block_type);
        self.marker_stack.push(block_type);
        // Parse the block
        match self.code[self.pos].clone() {
            lexer::LeftBrace => {
                self.pos += 1;
                // Loop through all the statements in the block
                loop {
                    match self.code[self.pos].clone() {
                        lexer::RightBrace => break,
                        _ => statements.push(self.parse_statement()),
                    }
                }
            },
            xx => fail!("Error: expected `LeftBrace` but found `{:?}`", xx)
        }
        // Get the block markers
        let (_, end_id) = self.marker_stack.pop();
        let (_, start_id) = self.marker_stack.pop();
        trans::Block {
            statements: statements,
            start_id: start_id,
            end_id: end_id,
        }
    }

    fn parse_statement(&mut self) -> trans::Statement {
        match self.code[self.pos].clone() {
            lexer::Let => self.parse_let(),
            lexer::If => self.parse_if(),
            lexer::For => fail!("ICE: Unimplemented"),
            lexer::While => fail!("ICE: Unimplemented"),
            lexer::Fn => fail!("ICE: Unimplemented"),
            lexer::Loop => self.parse_loop(),
            lexer::Ident(ref s) => self.parse_ident_statement(s),
            xx => fail!("Unexpected token found: `{:?}`", xx)
        }
    }

    fn parse_let(&mut self) -> trans::Statement {
        self.pos += 1;
        // Determine the variables name
        let var_name = match self.code[self.pos].clone() {
            lexer::Ident(ref s) => s.clone(),
            xx => fail!("Error: expected `Ident` but found `{:?}`", xx)
        };
        // Check if variable is already defined
        if SYSTEM_CALLS.iter().any(|&x| x == var_name) {
            fail!("Error: `{}` is a system command");
        }
        if self.variables.contains_key(&var_name) {
            fail!("Error: Shadowing is unsupported. (`{}` was already defined)", var_name);
        }
        // Add the variable to the variable list
        self.variables.insert(var_name.clone(), self.var_index);
        self.var_index += 1;
        self.pos += 1;
        // Check if we are assigning to the variable
        match self.code[self.pos].clone() {
            lexer::Assignment => {
                self.pos += 1;
                // Parse the assignment
                self.parse_assignment(self.var_index-1)
            },
            lexer::StatementEnd => {
                self.pos += 1;
                // Variable declaration without assignment is a NOP
                trans::Nop
            },
            xx => fail!("Error: expected `Assignment` or `StatementEnd` but found `{:?}`", xx),
        }
    }

    fn parse_assignment(&mut self, var_id: VarId) -> trans::Statement {
        // Parse the rhs expression
        let expr = self.parse_expression();
        // Check for statement end
        match self.code[self.pos].clone() {
            lexer::StatementEnd => {},
            xx => fail!("Error: expected `StatementEnd` but found `{:?}`", xx)
        }
        self.pos += 1;
        trans::Assignment(var_id, expr)
    }

    fn parse_if(&mut self) -> trans::Statement {
        self.pos += 1;
        // Parse the condition
        let condition_expr = self.parse_expression();
        // Parse the if block
        let if_block = self.parse_block(IfBlock);
        // Parse the else block
        let else_block = match self.code[self.pos].clone() {
            lexer::Else => {
                self.pos += 1;
                self.parse_block(IfBlock)
            },
            _ => {
                // Create a blank block if the else block was not specified
                self.marker_stack.last_marker += 2;
                trans::Block {
                    statements: ~[],
                    start_id: self.marker_stack.last_marker-2,
                    end_id: self.marker_stack.last_marker-1,
                }
            }
        };
        trans::If(condition_expr, if_block, else_block)
    }

    fn parse_loop(&mut self) -> trans::Statement {
        self.pos += 1;
        let mut loop_block = self.parse_block(LoopBlock);
        // Add the jump back to the start of the loop
        loop_block.statements.push(trans::Jump(loop_block.start_id));
        trans::Loop(loop_block)
    }

    fn parse_expression(&mut self) -> trans::Expression {
        let mut current_expr = None;
        loop {
            current_expr = match self.code[self.pos].clone() {
                lexer::Ident(ref s) => {
                    Some(trans::Variable(self.get_variable(s)))
                },
                lexer::LitNum(n) => {
                    self.pos += 1;
                    Some(trans::LitNum(n as u8))
                },
                lexer::Plus => Some(self.parse_operator(current_expr, trans::Plus)),
                lexer::Minus => Some(self.parse_operator(current_expr, trans::Minus)),
                _ => break,
            };
        }
        // Return the expression if there was one
        match current_expr {
            Some(expr) => expr,
            None => fail!("Error: expected expression")
        }
    }

    fn parse_operator(&mut self,
            lhs: Option<trans::Expression>,
            operator: trans::Operator) -> trans::Expression {
        // Check that there is a valid lhs expression
        let lhs = match lhs {
            Some(expr) => expr,
            None => fail!("Error: invalid LHS operand"),
        };
        self.pos += 1;
        // Parse the rhs expression
        let rhs = self.parse_expression();
        trans::OperatorExpr(~rhs, operator, ~lhs)
    }

    fn parse_ident_statement(&mut self, name: &~str) -> trans::Statement {
        // Check if the identifier was a system call
        match SYSTEM_CALLS.iter().find(|& &x| x == *name) {
            Some(&n) => {
                self.pos += 1;
                return trans::CallSys(match n {
                    "__clear"     => self.parse_syscall_clear(),
                    "__draw_pos"  => self.parse_syscall_draw_pos(),
                    "__set_addr"  => fail!("Unimplemented"),
                    "__draw"      => self.parse_syscall_draw(),
                    "__random"    => fail!("Unimplemented"),
                    "__set_delay" => fail!("Unimplemented"),
                    "__get_font"  => self.parse_syscall_get_font(),
                    "__key_wait"  => self.parse_syscall_key_wait(),

                    _ => fail!("Unreachable")
                });
            },
            None => {}
        }

        // Check if the identifier is a variable
        match self.variables.find(name) {
            Some(&var_id) => {
                self.pos += 1;
                match self.code[self.pos].clone() {
                    // If the identifier was a variable, then there should be an assignment
                    lexer::Assignment => {
                        self.pos += 1;
                        return self.parse_assignment(var_id);
                    },
                    xx => fail!("Error: expected `Assignment` but found `{:?}`", xx)
                }
            },
            None => {}
        }

        fail!("Error: `{}` is undefined", *name);
    }

    fn parse_syscall_clear(&mut self) -> trans::SysCall {
        if self.code[self.pos].clone() != lexer::LeftParen {
            fail!("Expected `LeftParen` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        if self.code[self.pos].clone() != lexer::RightParen {
            fail!("Expected `RightParen` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        if self.code[self.pos].clone() != lexer::StatementEnd {
            fail!("Expected `StatementEnd` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        trans::Clear
    }

    fn parse_syscall_draw_pos(&mut self) -> trans::SysCall {
        if self.code[self.pos].clone() != lexer::LeftParen {
            fail!("Expected `LeftParen` but found `{:?}`", self.code[self.pos]);
        }
        self.pos += 1;
        let x_expr = self.parse_expression();
        if self.code[self.pos].clone() != lexer::Comma {
            fail!("Expected `Comma` but found `{:?}`", self.code[self.pos]);
        }
        self.pos += 1;
        let y_expr = self.parse_expression();
        if self.code[self.pos].clone() != lexer::RightParen {
            fail!("Expected `RightParen` but found `{:?}`", self.code[self.pos]);
        }
        self.pos += 1;
        if self.code[self.pos].clone() != lexer::StatementEnd {
            fail!("Expected `StatementEnd` but found `{:?}`", self.code[self.pos]);
        }
        self.pos += 1;
        trans::DrawPos(x_expr, y_expr)
    }

    fn parse_syscall_draw(&mut self) -> trans::SysCall {
        if self.code[self.pos].clone() != lexer::LeftParen {
            fail!("Expected `LeftParen` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        let command = match self.code[self.pos].clone() {
            lexer::LitNum(n) => {
                assert!(n < 0x10);
                trans::Draw(n as u8)
            },
            xx => fail!("Expected `LitNum` but found `{:?}`", xx)
        };
        self.pos += 1;
        if self.code[self.pos].clone() != lexer::RightParen {
            fail!("Expected `RightParen` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        if self.code[self.pos].clone() != lexer::StatementEnd {
            fail!("Expected `StatementEnd` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        command
    }

    fn parse_syscall_get_font(&mut self) -> trans::SysCall {
        if self.code[self.pos].clone() != lexer::LeftParen {
            fail!("Expected `LeftParen` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        let expr = self.parse_expression();
        if self.code[self.pos].clone() != lexer::RightParen {
            fail!("Expected `RightParen` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        if self.code[self.pos].clone() != lexer::StatementEnd {
            fail!("Expected `StatementEnd` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        trans::GetFont(expr)
    }

    fn parse_syscall_key_wait(&mut self) -> trans::SysCall {
        if self.code[self.pos].clone() != lexer::LeftParen {
            fail!("Expected `LeftParen` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        let expr = self.parse_expression();
        if self.code[self.pos].clone() != lexer::RightParen {
            fail!("Expected `RightParen` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        if self.code[self.pos].clone() != lexer::StatementEnd {
            fail!("Expected `StatementEnd` but found {:?}", self.code[self.pos]);
        }
        self.pos += 1;
        trans::KeyWait(expr)
    }

    /// Returns the id of a variable name
    fn get_variable(&mut self, var_name: &~str) -> VarId {
        let var_id = match self.variables.find(var_name) {
            Some(id) => id,
            None => fail!("Error: `{}` is undefined", *var_name)
        };
        self.pos += 1;
        *var_id
    }
}
