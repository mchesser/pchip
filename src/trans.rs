use parser::ast;
use parser::asm;

#[deriving(Show, Clone)]
pub enum Address {
    VariableAddress(ast::VarId),
    FunctionAddress(ast::FnId),
    MarkerAddress(ast::MarkerId),
    RawAddress(u16)
}

#[deriving(Show, Clone)]
pub enum Operation {
    RawOp(asm::Operation),
    UnknownAddr(asm::Operation, Address),
    Marker(ast::MarkerId),
}

#[deriving(Show)]
pub struct Function {
    id: ast::FnId,
    code: ~[Operation],
    inline: bool,
}

pub fn trans_function(function: ast::Function) -> Function {
    Function {
        id: function.id,
        code: trans_block(function.body),
        inline: false,
    }
}

pub fn trans_block(block: ast::Block) -> ~[Operation] {
    let start_id = block.marker.start;
    let end_id = block.marker.end;
    let mut acc = ~[Marker(start_id)];
    for statement in block.statements.move_iter() {
        acc.push_all_move(trans_expression(statement.expr));
    }
    acc.push(Marker(end_id));
    acc
}

pub fn trans_expression(expr: ast::Expr) -> ~[Operation] {
    match expr {
        ast::If(~cond, ~then_block, ~else_block) => {
            // If
            let mut ops = trans_expression(cond);
            ops.push(RawOp(asm::CompareV(0x0, 0x0)));
            ops.push(UnknownAddr(asm::Jump(0), MarkerAddress(else_block.marker.start)));
            // Then
            ops.push_all_move(trans_block(then_block));
            ops.push(UnknownAddr(asm::Jump(0), MarkerAddress(else_block.marker.end)));
            // Else
            ops.push_all_move(trans_block(else_block));

            ops
        },
        ast::Loop(~block) => {
            trans_block(block)
        },
        ast::Assignment(var, ~rhs) => {
            let mut ops = trans_expression(rhs);
            ops.push(UnknownAddr(asm::SetAddress(0), VariableAddress(var)));
            ops.push(RawOp(asm::Write(0x0)));
            ops
        },
        ast::Call(id, call_vars, args) => {
            let mut ops = ~[];
            for (&store_id, arg) in call_vars.iter().zip(args.move_iter()) {
                let rtype = arg.rtype;

                if rtype != ast::AddressType {
                    ops.push_all_move(trans_expression(ast::Assignment(store_id, ~arg.expr)));
                }
                else {
                    // Need to special case address types because it is impossible to store them
                    // in memory
                    ops.push_all_move(trans_expression(arg.expr));
                }
            }
            ops.push(UnknownAddr(asm::Call(0), FunctionAddress(id)));
            ops
        },
        ast::AsmOperation(op) => {
            ~[RawOp(op)]
        },
        ast::Jump(id) => {
            ~[UnknownAddr(asm::Jump(0), MarkerAddress(id))]
        },
        ast::LitNum(n) => {
            ~[RawOp(asm::SetV(0x0, n))]
        },
        ast::Variable(id) => {
            ~[
                UnknownAddr(asm::SetAddress(0), VariableAddress(id)),
                RawOp(asm::Read(0x0)),
            ]
        },
        ast::Nop => {
            ~[]
        }
    }
}
