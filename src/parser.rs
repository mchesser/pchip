use ast;
use error::{InputPos, InputSpan, Logger};
use lexer;
use lexer::{Lexer, Token};

pub struct Parser<'a> {
    tokens: Vec<lexer::Token>,
    logger: Logger<'a>,
    index: uint,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer, logger: Logger<'a>) -> Parser<'a> {
        let mut tokens: Vec<lexer::Token> = lexer.collect();
        let end_token = lexer::Token {
            value: lexer::Eof,
            pos: tokens.last().map(|t| t.pos.clone()).unwrap_or(InputPos::start()),
        };
        tokens.push(end_token);
        Parser {
            tokens: tokens,
            logger: logger,
            index: 0,
        }
    }

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
        if  next != token {
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

    pub fn parse(&mut self) -> ast::Program {
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
            println!("{}\n", items.last());
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
            lexer::Let => ast::LetItem(self.parse_let()),

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

        let opt_assignment = match self.next_token() {
            lexer::Assignment => Some(self.parse_assignment(name.clone())),
            lexer::SemiColon => None,
            invalid => {
                self.logger.report_error(format!("expected `=` or `;` but found `{}`",
                    invalid), InputSpan::new(span_start, self.current_pos()));
                self.fatal_error();
            }
        };

        // If the type wasn't specified for this variable then there needs
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
            lexer::Bool => ast::Primitive(ast::BoolType),

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
                    self.expect(lexer::SemiColon);
                    statements.push(expression);
                }
            }
        }

        println!("Parsed Function, next_token: {}", self.peek());

        ast::Block {
            statements: statements,
            span: InputSpan::new(span_start, self.current_pos()),
        }
    }

    fn parse_expression(&mut self) -> ast::Expression {
        let span_start = self.current_pos();

        match self.next_token() {
            lexer::Ident(name) => self.handle_ident(name.to_string()),
            lexer::LitNum(value) => self.handle_num(value),
            lexer::Let => {
                ast::Expression {
                    expr: box ast::LetExpr(self.parse_let()),
                    rtype: ast::Primitive(ast::UnitType),
                }
            },
            lexer::If => self.parse_if(),
            lexer::For => unimplemented!(),
            lexer::While => unimplemented!(),
            lexer::Loop => self.parse_loop(),
            lexer::Break => self.parse_break(),
            invalid => {
                let span_end = self.current_pos();
                self.logger.report_error(format!("expected `<Expression>` but found `{}`", invalid),
                    InputSpan::new(span_start, span_end));
                self.fatal_error();
            },
        }
    }

    fn handle_ident(&mut self, name: String) -> ast::Expression {
        match self.peek() {
            lexer::LeftParen => {
                self.bump();
                self.parse_call(name)
            },

            lexer::Assignment => {
                ast::Expression {
                    expr: box ast::AssignExpr(self.parse_assignment(name)),
                    rtype: ast::Primitive(ast::UnitType),
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
                }
            }
        }
    }

    fn parse_call(&mut self, name: String) -> ast::Expression {
        let span_start = self.current_pos();
        let mut args = vec![];
        loop {
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

    fn handle_num(&mut self, val: int) -> ast::Expression {
        ast::Expression {
            expr: box ast::LitNumExpr(val),
            rtype: ast::Primitive(ast::IntType),
        }
    }

    fn parse_if(&mut self) -> ast::Expression {
        let span_start = self.current_pos();

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
        }
    }

    fn parse_loop(&mut self) -> ast::Expression {
        let span_start = self.current_pos();
        let body = self.parse_block();

        let loop_statement = ast::LoopStatement {
            body: body,
            span: InputSpan::new(span_start, self.current_pos()),
        };

        ast::Expression {
            expr: box ast::LoopExpr(loop_statement),
            rtype: ast::Primitive(ast::UnitType),
        }
    }

    fn parse_break(&mut self) -> ast::Expression {
        ast::Expression {
            expr: box ast::Break,
            rtype: ast::Primitive(ast::BottomType),
        }
    }
}
