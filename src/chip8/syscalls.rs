use parser::ast;
use parser::asm;
use parser::Parser;

/// Register Chip8 system calls with the parser
pub fn register_system_calls(parser: &mut Parser) {
    // clear()
    let marker = parser.marker_stack.get();
    parser.register_function(
        "clear", vec![],
        ast::Block {
            statements: vec![
                ast::Expression {
                    expr: ast::AsmOperation(asm::Clear),
                    rtype: ast::UnitType
                },
            ],
            marker: marker,
        }
    );

    // draw_pos(x, y)
    let marker = parser.marker_stack.get();
    let (arg0, arg1) = (parser.next_var(0), parser.next_var(1));
    parser.register_function(
        "draw_pos", vec![ast::U8Type, ast::U8Type],
        ast::Block {
            statements: vec![
                ast::Expression {
                    expr: ast::Variable(arg0),
                    rtype: ast::U8Type,
                },
                ast::Expression {
                    expr: ast::AsmOperation(asm::Set(0xD, 0x0)),
                    rtype: ast::UnitType,
                },
                ast::Expression {
                    expr: ast::Variable(arg1),
                    rtype: ast::U8Type,
                },
                ast::Expression {
                    expr: ast::AsmOperation(asm::Set(0xE, 0x0)),
                    rtype: ast::UnitType,
                },
            ],
            marker: marker,
        }
    );

    // draw5(glyph_address)
    let marker = parser.marker_stack.get();
    parser.register_function(
        "draw5", vec![ast::AddressType],
        ast::Block {
            statements: vec![
                ast::Expression {
                    expr: ast::AsmOperation(asm::Draw(0xD, 0xE, 5)),
                    rtype: ast::UnitType,
                },
            ],
            marker: marker,
        }
    );

    // get_font(char_code)
    let marker = parser.marker_stack.get();
    let arg0 = parser.next_var(0);
    parser.register_function(
        "get_font", vec![ast::U8Type],
        ast::Block {
            statements: vec![
                ast::Expression {
                    expr: ast::Variable(arg0),
                    rtype: ast::U8Type,
                },
                ast::Expression {
                    expr: ast::AsmOperation(asm::GetFont(0x0)),
                    rtype: ast::AddressType,
                },
            ],
            marker: marker,
        }
    );

    // key_wait()
    let marker = parser.marker_stack.get();
    parser.register_function(
        "key_wait", vec![],
        ast::Block {
            statements: vec![
                ast::Expression {
                    expr: ast::AsmOperation(asm::KeyWait(0x0)),
                    rtype: ast::U8Type,
                },
            ],
            marker: marker,
        }
    );

    // plus(a, b)
    let marker = parser.marker_stack.get();
    let (arg0, arg1) = (parser.next_var(0), parser.next_var(1));
    parser.register_function(
        "plus", vec![ast::U8Type, ast::U8Type],
        ast::Block {
            statements: vec![
                ast::Expression {
                    expr: ast::Variable(arg0),
                    rtype: ast::U8Type,
                },
                ast::Expression {
                    expr: ast::AsmOperation(asm::Set(0x1, 0x0)),
                    rtype: ast::UnitType,
                },
                ast::Expression {
                    expr: ast::Variable(arg1),
                    rtype: ast::U8Type,
                },
                ast::Expression {
                    expr: ast::AsmOperation(asm::Add(0x0, 0x1)),
                    rtype: ast::U8Type,
                },
            ],
            marker: marker,
        }
    );

    // minus(a, b)
    let marker = parser.marker_stack.get();
    let (arg0, arg1) = (parser.next_var(0), parser.next_var(1));
    parser.register_function(
        "minus", vec![ast::U8Type, ast::U8Type],
        ast::Block {
            statements: vec![
                ast::Expression {
                    expr: ast::Variable(arg0),
                    rtype: ast::U8Type,
                },
                ast::Expression {
                    expr: ast::AsmOperation(asm::Set(0x1, 0x0)),
                    rtype: ast::UnitType,
                },
                ast::Expression {
                    expr: ast::Variable(arg1),
                    rtype: ast::U8Type,
                },
                ast::Expression {
                    expr: ast::AsmOperation(asm::Sub(0x0, 0x1)),
                    rtype: ast::U8Type,
                },
            ],
            marker: marker,
        }
    );
}
