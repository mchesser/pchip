use ast;
use error::{InputPos, InputSpan, Logger};
use lexer;
use lexer::{Lexer, Token};

pub fn parse<'a>(mut lexer: Lexer, logger: &'a Logger<'a>) -> ast::Program {
    let mut tokens: Vec<lexer::Token> = lexer.collect();
    let end_token = lexer::Token {
        value: lexer::Eof,
        pos: tokens.last().map(|t| t.pos.clone()).unwrap_or(InputPos::start()),
    };
    tokens.push(end_token);
    let mut parser = Parser {
        tokens: tokens,
        logger: logger,
        index: 0,
        fake_semicolon: false,
    };
    parser.parse()
}

struct Parser<'a> {
    tokens: Vec<lexer::Token>,
    logger: &'a Logger<'a>,
    index: uint,
    fake_semicolon: bool,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> lexer::TokenValue {
        self.tokens[self.index].value.clone()
    }

    fn next_token(&mut self) -> lexer::TokenValue {
        self.index += 1;
        self.tokens[self.index - 1].value.clone()
    }

    fn bump(&mut self) {
        self.index += 1;
    }

    fn expect(&mut self, token: lexer::TokenValue) {
        let span_start = self.current_pos();
        let next = self.next_token();
        if next != token {
            self.logger.report_error(format!("expected `{}` but found `{}`", token, next),
                InputSpan::new(span_start, self.current_pos()));
            self.fatal_error();
        }
    }

    fn fatal_error(&self) -> ! {
        fail!("");
    }

    fn current_pos(&self) -> InputPos {
        self.tokens[self.index].pos.clone()
    }

    fn parse(&mut self) -> ast::Program {
        let mut items = vec![];
        let span_start = self.current_pos();
        let mut span_end = self.current_pos();

        loop {
            match self.try_parse_item() {
                Some(item) => {
                    span_end = item.span().end;
                    items.push(item);
                },
                None => break,
            }
        }

        ast::Program {
            items: items,
            span: InputSpan::new(span_start, span_end),
        }
    }

    fn try_parse_item(&mut self) -> Option<ast::Item> {
        let span_start = self.current_pos();
        let item = match self.next_token() {
            lexer::Fn => ast::FunctionItem(self.parse_function()),
            lexer::Struct => ast::StructItem(self.parse_struct_decl()),
            lexer::Let => {
                let item = ast::LetItem(self.parse_let());
                self.expect(lexer::SemiColon);
                item
            },

            lexer::Eof => return None,

            invalid => {
                self.logger.report_error(format!("expected `<Item>` but found, `{}`", invalid),
                    InputSpan::new(span_start, self.current_pos()));
                self.fatal_error();
            },
        };

        Some(item)
    }

    fn parse_function(&mut self) -> ast::FunctionDeclaration {
        let span_start = self.current_pos();

        // Read function name
        let name = self.parse_name();

        // Read function args
        self.expect(lexer::LeftParen);
        let mut params = vec![];

        // Handle case with 0 args
        if self.peek() == lexer::RightParen {
            self.bump();
        }
        // Otherwise parse all the parameters
        else {
            loop {
                let param = self.parse_param();
                params.push(param);

                // Check if there is another argument
                if self.peek() == lexer::Comma {
                    self.bump()
                }
                // Otherwise check for closing paren as break
                else {
                    self.expect(lexer::RightParen);
                    break;
                }
            }
        }

        let type_span_start = self.current_pos();
        // Read function return type
        let rtype = match self.peek() {
            lexer::LeftBrace => ast::Primitive(ast::UnitType),

            lexer::RightArrow => {
                self.bump();
                let type_ = self.parse_type();
                type_
            },

            invalid => {
                self.logger.report_error(format!("expected `{{` or `->` but found `{}`", invalid),
                    InputSpan::new(type_span_start, self.current_pos()));
                self.fatal_error();
            },
        };

        // Read function body
        let body = self.parse_block();

        let span_end = self.current_pos();
        ast::FunctionDeclaration {
            name: name,
            params: params,
            rtype: rtype,
            body: body,
            span: InputSpan::new(span_start, span_end),
        }
    }

    fn parse_struct_decl(&mut self) -> ast::StructDeclaration {
        unimplemented!()
    }

    fn parse_let(&mut self) -> ast::LetStatement {
        let span_start = self.current_pos();
        let (name, opt_type) = self.parse_var_with_type();

        let opt_assignment = match self.peek() {
            lexer::Assignment => {
                self.bump();
                Some(self.parse_assignment(name.clone()))
            },
            lexer::SemiColon => None,
            invalid => {
                self.logger.report_error(format!("expected `=` or `;` but found `{}`",
                    invalid), InputSpan::new(span_start, self.current_pos()));
                self.fatal_error();
            }
        };

        // If the type wasn't specified for this variable then there needs an assignment
        let type_ = match (&opt_type, &opt_assignment) {
            (&Some(ref t), _) => t.clone(),
            (&None, &Some(ref assignment)) => {
                assignment.expression.rtype.clone()
            },
            (&None, &None) => {
                self.logger.report_error(format!("could not determine type for variable `{}`",
                    name), InputSpan::new(span_start, self.current_pos()));
                self.fatal_error();
            },
        };

        ast::LetStatement {
            name: name,
            var_type: type_,
            assignment: opt_assignment,
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_name(&mut self) -> String {
        let span_start = self.current_pos();
        match self.next_token() {
            lexer::Ident(name) => name.clone(),

            invalid => {
                self.logger.report_error(format!("expected `<identifer>` but found `{}`", invalid),
                    InputSpan::new(span_start, self.current_pos()));
                self.fatal_error();
            },
        }
    }

    fn parse_type(&mut self) -> ast::Type {
        let span_start = self.current_pos();
        match self.next_token() {
            lexer::Ident(name) => ast::UserType(name.clone()),
            lexer::Int => ast::Primitive(ast::IntType),
            lexer::Char => ast::Primitive(ast::CharType),
            lexer::Bool => ast::Primitive(ast::BoolType),
            lexer::Any => ast::Primitive(ast::AnyType),
            lexer::Star => ast::Pointer(box self.parse_type()),

            invalid => {
                self.logger.report_error(format!("expected `<Type>` but found `{}`", invalid),
                    InputSpan::new(span_start, self.current_pos()));
                self.fatal_error();
            },
        }
    }

    fn parse_param(&mut self) -> (String, ast::Type) {
        let span_start = self.current_pos();
        match self.parse_var_with_type() {
            (name, Some(t)) => (name, t),
            (name, None) => {
                let span_end = self.current_pos();
                self.logger.report_error(format!("expected `<Type>` for parameter `{}`", name),
                    InputSpan::new(span_start, span_end));
                self.fatal_error();
            },
        }
    }

    fn parse_var_with_type(&mut self) -> (String, Option<ast::Type>) {
        let name = self.parse_name();

        match self.peek() {
            lexer::Colon => self.bump(),
            _ => return (name, None),
        }

        let type_ = self.parse_type();
        (name, Some(type_))
    }

    fn parse_block(&mut self) -> ast::Block {
        let span_start = self.current_pos();
        self.expect(lexer::LeftBrace);

        let mut statements = vec![];
        loop {
            if self.peek() == lexer::RightBrace {
                self.bump();
                break;
            }
            else {
                let expression = self.parse_expression();
                if self.peek() != lexer::RightBrace {
                    if !self.fake_semicolon {
                        self.expect(lexer::SemiColon);
                    }
                    self.fake_semicolon = false;
                    statements.push(expression);
                }
                else {
                    statements.push(expression);
                }
            }
        }

        ast::Block {
            statements: statements,
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_expression(&mut self) -> ast::Expression {
        let span_start = self.current_pos();
        let mut expression = match self.next_token() {
            lexer::Star => {
                let deref_target = self.parse_expression();
                let rtype = deref_target.rtype.clone();
                ast::Expression {
                    expr: box ast::DerefExpr(deref_target),
                    rtype: ast::DerefType(box rtype),
                    span: InputSpan::new(span_start, self.current_pos()),
                }
            },
            lexer::Ident(name) => self.handle_ident(name.to_string(), span_start),
            lexer::LitNum(value) => self.handle_num(value, span_start),
            lexer::True => {
                ast::Expression {
                    expr: box ast::LitNumExpr(1),
                    rtype: ast::Primitive(ast::BoolType),
                    span: InputSpan::new(span_start, self.current_pos()),
                }
            },
            lexer::False => {
                ast::Expression {
                    expr: box ast::LitNumExpr(0),
                    rtype: ast::Primitive(ast::BoolType),
                    span: InputSpan::new(span_start, self.current_pos()),
                }
            },
            lexer::Minus => {
                match self.peek() {
                    lexer::LitNum(value) => {
                        self.bump();
                        self.handle_num(-value, span_start)
                    },
                    _ => fail!("ICE: Cannot negate expression")
                }
            }
            lexer::Let => {
                ast::Expression {
                    expr: box ast::LetExpr(self.parse_let()),
                    rtype: ast::Primitive(ast::UnitType),
                    span: InputSpan::new(span_start, self.current_pos()),
                }
            },
            lexer::If => self.parse_if(span_start),
            lexer::For => unimplemented!(),
            lexer::While => self.parse_while(span_start),
            lexer::Loop => self.parse_loop(span_start),
            lexer::Break => self.parse_break(span_start),
            lexer::Return => self.parse_return(span_start),
            lexer::Asm => self.parse_asm(span_start),
            invalid => {
                self.logger.report_error(format!("expected `<Expression>` but found `{}`", invalid),
                    InputSpan::new(span_start, self.current_pos()));
                self.fatal_error();
            },
        };

        // Check for a type cast
        match self.peek() {
            lexer::As => {
                self.bump();
                expression.rtype = self.parse_type();
            },
            _ => {},
        }
        expression
    }

    fn handle_ident(&mut self, name: String, span_start: InputPos) -> ast::Expression {
        match self.peek() {
            lexer::LeftParen => {
                self.bump();
                self.parse_call(name, span_start)
            },

            lexer::Assignment => {
                self.bump();
                ast::Expression {
                    expr: box ast::AssignExpr(self.parse_assignment(name)),
                    rtype: ast::Primitive(ast::UnitType),
                    span: InputSpan::new(span_start, self.current_pos()),
                }
            }

            lexer::Plus => unimplemented!(),
            lexer::Minus => unimplemented!(),
            lexer::PlusEq => unimplemented!(),
            lexer::MinusEq => unimplemented!(),

            _ => {
                ast::Expression {
                    expr: box ast::VariableExpr(name.clone()),
                    rtype: ast::VariableType(name),
                    span: InputSpan::new(span_start, self.current_pos()),
                }
            }
        }
    }

    fn parse_call(&mut self, name: String, span_start: InputPos) -> ast::Expression {
        let mut args = vec![];
        loop {
            if self.peek() == lexer::RightParen {
                self.bump();
                break;
            }

            args.push(self.parse_expression());

            // Check if there is another argument
            if self.peek() == lexer::Comma {
                self.bump()
            }
            // Otherwise check for closing paren as break
            else {
                self.expect(lexer::RightParen);
                break;
            }
        }

        let function_call = ast::FunctionCall {
            name: name.clone(),
            args: args,
            span: InputSpan::new(span_start, self.current_pos()),
        };

        ast::Expression {
            expr: box ast::CallExpr(function_call),
            rtype: ast::VariableType(name),
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_assignment(&mut self, target: String) -> ast::Assignment {
        let span_start = self.current_pos();
        ast::Assignment {
            target: target,
            expression: self.parse_expression(),
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn handle_num(&mut self, val: int, span_start: InputPos) -> ast::Expression {
        ast::Expression {
            expr: box ast::LitNumExpr(val),
            rtype: ast::Primitive(ast::IntType),
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_if(&mut self, span_start: InputPos) -> ast::Expression {
        let condition = self.parse_expression();
        let body = self.parse_block();
        let else_block = match self.peek() {
            lexer::Else => {
                self.bump();
                if self.next_token() == lexer::If {
                    fail!("FIXME: Handle else if statements");
                }

                Some(self.parse_block())
            },
            _ => None,
        };

        // Insert an implicit semicolon if there wasn't one at the end of the if statement
        if self.peek() != lexer::SemiColon {
            self.fake_semicolon = true;
        }

        let if_statement = ast::IfStatement {
            condition: condition,
            body: body,
            else_block: else_block,
            span: InputSpan::new(span_start, self.current_pos()),
        };

        let rtype = if_statement.body.rtype();

        ast::Expression {
            expr: box ast::IfExpr(if_statement),
            rtype: rtype,
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_loop(&mut self, span_start: InputPos) -> ast::Expression {
        let body = self.parse_block();

        // Insert an implicit semicolon if there wasn't one at the end of the loop statement
        if self.peek() != lexer::SemiColon {
            self.fake_semicolon = true;
        }

        let loop_statement = ast::LoopStatement {
            body: body,
            span: InputSpan::new(span_start, self.current_pos()),
        };

        ast::Expression {
            expr: box ast::LoopExpr(loop_statement),
            rtype: ast::Primitive(ast::BottomType),
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_while(&mut self, span_start: InputPos) -> ast::Expression {
        // Transform the while statement into a loop with a if statement
        let condition = self.parse_expression();
        let break_body = ast::Block {
            statements: vec![ast::Expression {
                expr: box ast::Break,
                rtype: ast::Primitive(ast::BottomType),
                span: InputSpan::invalid(),
            }],
            span: InputSpan::new(span_start, self.current_pos()),
        };

        let loop_body = self.parse_block();
        let body_span = loop_body.span.clone();

        let real_body = ast::IfStatement {
            condition: condition,
            body: loop_body,
            else_block: Some(break_body),
            span: InputSpan::new(span_start, self.current_pos()),
        };

        // Insert an implicit semicolon if there wasn't one at the end of the while statement
        if self.peek() != lexer::SemiColon {
            self.fake_semicolon = true;
        }

        let loop_statement = ast::LoopStatement {
            body: ast::Block {
                statements: vec![ast::Expression {
                    expr: box ast::IfExpr(real_body),
                    rtype: ast::Primitive(ast::BottomType),
                    span: InputSpan::invalid(),
                }],
                span: body_span,
            },
            span: InputSpan::new(span_start, self.current_pos()),
        };

        ast::Expression {
            expr: box ast::LoopExpr(loop_statement),
            rtype: ast::Primitive(ast::BottomType),
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_break(&mut self, span_start: InputPos) -> ast::Expression {
        ast::Expression {
            expr: box ast::Break,
            rtype: ast::Primitive(ast::BottomType),
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_return(&mut self, span_start: InputPos) -> ast::Expression {
        let expression = self.parse_expression();
        let rtype = expression.rtype.clone();
        ast::Expression {
            expr: box ast::Return(expression),
            rtype: ast::Primitive(ast::BottomType),
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_asm(&mut self, span_start: InputPos) -> ast::Expression {
        self.expect(lexer::LeftBrace);

        let mut code = String::new();
        loop {
            match self.next_token() {
                lexer::LitString(ref string) => {
                    code.push_str(string.as_slice());
                },
                lexer::RightBrace => {
                    code.pop_char();
                    break;
                },
                invalid => {
                    self.logger.report_error(
                        format!("expected `\"<string>\"` or `}}` but found, `{}`", invalid),
                        InputSpan::new(span_start, self.current_pos()));
                    self.fatal_error();
                },
            }

            match self.next_token() {
                lexer::RightBrace => break,
                lexer::Comma => {
                    code.push_char('\n');
                    continue;
                },
                invalid => {
                    self.logger.report_error(
                        format!("expected `,` or `}}` but found, `{}`", invalid),
                        InputSpan::new(span_start, self.current_pos()));
                    self.fatal_error();
                },
            }
        }

        ast::Expression {
            expr: box ast::AsmOpExpr(code),
            rtype: ast::Primitive(ast::AnyType),
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }
}
