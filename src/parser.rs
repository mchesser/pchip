extern crate collections;

use parser::collections::HashMap;

mod lexer;
mod ast;
mod trans;
mod asm;
mod chip8;

struct MarkerStack {
    markers: Vec<(ast::BlockType, ast::Marker)>,
    last_marker: ast::MarkerId,
}

impl MarkerStack {
    fn new() -> MarkerStack {
        MarkerStack {
            markers: Vec::new(),
            last_marker: 0
        }
    }

    /// Add a new marker to the marker stack
    fn add(&mut self, block_type: ast::BlockType) {
        let marker = ast::Marker {
            start: self.last_marker,
            end: self.last_marker + 1,
        };
        self.markers.push((block_type, marker));
        self.last_marker += 2;
    }

    /// Get what the next marker would be
    fn get(&mut self) -> ast::Marker {
        self.last_marker += 2;
        ast::Marker {
            start: self.last_marker - 2,
            end: self.last_marker - 1,
        }
    }

    /// Pop the last marker added
    fn pop(&mut self) -> ast::Marker {
        match self.markers.pop() {
            Some((_, marker)) => marker,
            None => fail!("ICE: Marker stack was empty when pop was called"),
        }
    }

    /// Find the top most marker of the specified type
    fn find_type(&mut self, block_type: ast::BlockType) -> Option<ast::Marker> {
        self.markers.iter().rev().find(|&&(btype, _)| btype == block_type).map(|&(_, id)| id)
    }
}

fn check_expr_type(expr: &ast::Expression, rtype: ast::ReturnType) {
    assert!(expr.rtype == rtype);
}

fn check_block_type(block: &ast::Block, rtype: ast::ReturnType) {
    assert!(block.return_type() == rtype);
}

enum BlockItem {
    Statement(ast::Expression),
    Expression(ast::Expression),
    BlockEnd,
}

pub struct Parser {
    variables: HashMap<~str, ast::Variable>,
    var_index: ast::VarId,
    functions: HashMap<~str, ast::Function>,
    fn_index: ast::FnId,
    marker_stack: MarkerStack,
    code: Vec<lexer::Token>,
    pos: uint,
}

pub fn parse(source_code: &str) -> Vec<u8> {
    let mut parser = Parser {
        variables: HashMap::new(),
        var_index: 0,
        functions: HashMap::new(),
        fn_index: 0,
        marker_stack: MarkerStack::new(),
        code: lexer::Lexer::new(source_code).collect(),
        pos: 0,
    };

    println!("--------------| INPUT | --------------")
    println!("{}", source_code);
    println!("");

    println!("--------------| LEXER | --------------");
    println!("{:?}", parser.code.as_slice());
    println!("");
    
    chip8::syscalls::register_system_calls(&mut parser);

    let main_block = parser.parse_block(ast::FunctionBlock);
    let main_id = parser.fn_index;
    parser.register_function("main", vec![], main_block);

    let num_vars = parser.var_index;
    let num_markers = parser.marker_stack.last_marker;

    println!("--------------|  AST  | --------------");

    for (name, function) in parser.functions.iter() {
        println!("FUNCTION: {}", name);
        println!("{}", function);
        println!("");
    }

    let functions: Vec<trans::Function> = parser.functions.move_iter()
            .map(|(_, function)| trans::trans_function(function)).collect();

    println!("--------------| TRANS | --------------");

    for function in functions.iter() {
        println!("{}", function);
        println!("");
    }

    println!("--------------|  ASM  | --------------");

    asm::compile(functions, main_id, num_vars, num_markers)
}

impl Parser {
    /// Gets the next token
    /// # Return
    /// Returns the next token
    fn next(&self) -> lexer::Token {
        self.code.get(self.pos).clone()
    }

    /// Checks the next token, stepping forward one token if correct and failing if it is incorrect
    /// # Arguments
    /// `token` - The correct token
    fn check(&mut self, token: lexer::Token) {
        if self.next() == token {
            self.pos += 1;
        }
        else {
            self.fail_expected(token);
        }
    }

    /// Fail for an unexpected token
    fn fail_unexpected_token(&mut self) -> ! {
        fail!("Error: `{}` is not allowed here", self.next())
    }

    /// Fail for expected token not found
    /// # Arguments
    /// `token` - the expected token
    fn fail_expected(&mut self, token: lexer::Token) -> ! {
        fail!("Error: expected `{}` but found `{}`", token, self.next())
    }

    /// Fail for undefined identifier
    fn fail_undefined(&mut self, name: &str) -> ! {
        fail!("Error: `{}` is undefined", name)
    }

    fn register_variable(&mut self, name: &str) {
        self.variables.insert(name.to_owned(), ast::Variable { id: self.var_index });
        self.var_index += 1;
    }
    
    fn next_var(&self, ahead: uint) -> ast::VarId {
        self.var_index + ahead
    }

    fn register_function(&mut self, name: &str, args: Vec<ast::ReturnType>, body: ast::Block) {
        let mut call_vars = Vec::new();
        for _ in args.iter() {
            call_vars.push(self.var_index);
            self.var_index += 1;
        }

        let function = ast::Function {
            arg_types: args,
            call_vars: call_vars,
            body: body,
            id: self.fn_index,
        };
        self.functions.insert(name.to_owned(), function);
        self.fn_index += 1;
    }

    /// Parse a block. A block is defined by a { and a }
    /// # Arguments
    /// `block_type` - The type of block. Only LoopBlocks can call break
    /// # Return
    /// Returns the ast representation of the block
    fn parse_block(&mut self, block_type: ast::BlockType) -> ast::Block {
        self.marker_stack.add(block_type);

        let mut statements = Vec::new();

        self.check(lexer::LeftBrace);

        loop {
            match self.parse_block_item() {
                Statement(s) => statements.push(s),
                Expression(e) => {
                    statements.push(e);
                    break;
                },
                BlockEnd => break,
            }
        }

        self.check(lexer::RightBrace);

        ast::Block {
            statements: statements,
            marker: self.marker_stack.pop(),
        }
    }

    fn parse_block_item(&mut self) -> BlockItem {
        if self.next() == lexer::RightBrace {
            return BlockEnd;
        }

        let expr = self.parse_expression();

        if self.next() == lexer::StatementEnd {
            self.pos += 1;
            Statement(expr)
        }
        else {
            Expression(expr)
        }
    }

    fn parse_expression(&mut self) -> ast::Expression {
        match self.next() {
            lexer::Ident(ref s) => self.parse_ident(s),
            lexer::LitNum(n)    => self.parse_lit(n),
            lexer::Let          => self.parse_let(),
            lexer::If           => self.parse_if(),
            lexer::For          => unimplemented!(),
            lexer::While        => unimplemented!(),
            lexer::Fn           => unimplemented!(),
            lexer::Loop         => self.parse_loop(),
            lexer::Break        => self.parse_break(),
            _                   => self.fail_unexpected_token(),
        }
    }

    fn parse_lit(&mut self, num: u16) -> ast::Expression {
        self.pos += 1;
        ast::Expression {
            expr: ast::LitNum(num as u8),
            rtype: ast::U8Type,
        }
    }

    fn parse_let(&mut self) -> ast::Expression {
        self.pos += 1;

        // Determine the variables name
        let var_name = match self.next() {
            lexer::Ident(ref s) => s.clone(),
            _ => self.fail_expected(lexer::Ident(~"<Identifier>")),
        };

        // Check if variable is already defined
        if self.functions.contains_key(&var_name) {
            fail!("Error: Shadowing is unsupported. (`{}` was already defined)", var_name);
        }
        if self.variables.contains_key(&var_name) {
            fail!("Error: Shadowing is unsupported. (`{}` was already defined)", var_name);
        }

        // Register the variable
        self.register_variable(var_name);
        self.pos += 1;

        // Check if we are assigning to the variable
        if self.next() == lexer::Assignment {
            self.pos += 1;
            self.parse_assignment(self.var_index-1)
        }
        else {
            // Not assigning anything to the varible doesn't require us to do anything
            ast::Expression {
                expr: ast::Nop,
                rtype: ast::UnitType,
            }
        }
    }

    fn parse_assignment(&mut self, var_id: ast::VarId) -> ast::Expression {
        let expr = self.parse_expression();
        check_expr_type(&expr, ast::U8Type);
        ast::Expression {
            expr: ast::Assignment(var_id, ~expr.expr),
            rtype: ast::UnitType,
        }
    }

    fn parse_if(&mut self) -> ast::Expression {
        self.pos += 1;

        let condition_expr = self.parse_expression();
        check_expr_type(&condition_expr, ast::BoolType);

        let if_block = self.parse_block(ast::IfBlock);
        let else_block = match self.next() {
            lexer::Else => {
                self.pos += 1;
                self.parse_block(ast::IfBlock)
            },
            _ => {
                // Create a blank block if the else block was not specified
                self.marker_stack.add(ast::IfBlock);
                ast::Block {
                    statements: vec![],
                    marker: self.marker_stack.pop(),
                }
            }
        };
        let return_type = if_block.return_type();

        // Check that if block and else block return the same type
        check_block_type(&else_block, return_type);

        ast::Expression {
            expr: ast::If(~condition_expr.expr, ~if_block, ~else_block),
            rtype: return_type,
        }
    }

    fn parse_loop(&mut self) -> ast::Expression {
        self.pos += 1;

        let mut loop_block = self.parse_block(ast::LoopBlock);

        // Add the jump back to the start of the loop
        loop_block.statements.push(ast::Expression {
            expr: ast::Jump(loop_block.marker.start),
            rtype: ast::UnitType,
        });

        ast::Expression {
            expr: ast::Loop(~loop_block),
            rtype: ast::UnitType,
        }
    }

    fn parse_break(&mut self) -> ast::Expression {
        let jump_marker = match self.marker_stack.find_type(ast::LoopBlock) {
            Some(id) => id,
            None     => self.fail_unexpected_token(),
        };

        self.pos += 1;

        ast::Expression {
            expr: ast::Jump(jump_marker.end),
            rtype: ast::UnitType,
        }
    }

    fn parse_ident(&mut self, name: &~str) -> ast::Expression {
        match self.try_parse_function(name) {
            Some(expr) => return expr,
            None => {},
        }

        match self.variables.find(name) {
            Some(&var) => {
                self.pos += 1;
                if self.next() == lexer::Assignment {
                    self.pos += 1;
                    return self.parse_assignment(var.id);
                }
                else {
                    return ast::Expression {
                        expr: ast::Variable(var.id),
                        rtype: ast::U8Type,
                    };
                }
            },
            None => {},
        }

        // The identifier is not defined anywhere so fail
        self.fail_undefined(*name);
    }

    fn try_parse_function(&mut self, name: &~str) -> Option<ast::Expression> {
        if self.functions.find(name).is_none() {
            return None;
        }

        self.pos += 1;

        let input_args = self.parse_function_args();

        let function = match self.functions.find(name) {
            Some(function) => function,
            None => unreachable!(),
        };

        // Check for the correct number of arguments
        if input_args.len() != function.arg_types.len() {
            fail!("Error: Incorrect number of arguments (expected `{}` but found `{}`)",
                    function.arg_types.len(), input_args.len());
        }

        // Check for correct return types
        for (in_arg, out_arg) in input_args.iter().zip(function.arg_types.iter()) {
            if in_arg.rtype != *out_arg {
                fail!("Error: Incorrect argument type (expected `{}` but found `{}`)",
                    in_arg.rtype, *out_arg);
            }
        }

        Some(ast::Expression {
            expr: ast::Call(function.id, function.call_vars.clone(), input_args),
            rtype: function.body.return_type(),
        })
    }

    fn parse_function_args(&mut self) -> Vec<ast::Expression> {
        self.check(lexer::LeftParen);

        // Check for no function args
        if self.next() == lexer::RightParen {
            self.pos += 1;
            return vec![];
        }

        let mut args = Vec::new();
        loop {
            args.push(self.parse_expression());
            if self.next() != lexer::Comma {
                break;
            }
            self.pos += 1;
        }
        self.check(lexer::RightParen);

        args
    }
}
