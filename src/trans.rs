use ast;
use asm;

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
    pub id: ast::FnId,
    pub code: Vec<Operation>,
    pub inline: bool,
}

pub fn trans_function(function: ast::Function) -> Function {
    Function {
        id: function.id,
        code: trans_block(function.body),
        inline: false,
    }
}

pub fn trans_block(block: ast::Block) -> Vec<Operation> {
    let start_id = block.marker.start;
    let end_id = block.marker.end;
    let mut acc = vec![Marker(start_id)];
    for statement in block.statements.move_iter() {
        acc.push_all(trans_expression(statement.expr).as_slice());
    }
    acc.push(Marker(end_id));
    acc
}

pub fn trans_expression(expr: ast::Expr) -> Vec<Operation> {
    let mut ops = Vec::new();
    match expr {
        ast::If(cond, then_block, else_block) => {
            // If
            ops.push_all(trans_expression(*cond).as_slice());
            ops.push(RawOp(asm::CompareV(0x0, 0x0)));
            ops.push(UnknownAddr(asm::Jump(0), MarkerAddress(else_block.marker.start)));
            // Then
            ops.push_all(trans_block(*then_block).as_slice());
            ops.push(UnknownAddr(asm::Jump(0), MarkerAddress(else_block.marker.end)));
            // Else
            ops.push_all(trans_block(*else_block).as_slice());
        },
        ast::Loop(block) => {
            ops.push_all(trans_block(*block).as_slice())
        },
        ast::Assignment(var, rhs) => {
            ops.push_all(trans_expression(*rhs).as_slice());
            ops.push(UnknownAddr(asm::SetAddress(0), VariableAddress(var)));
            ops.push(RawOp(asm::Write(0x0)));
        },
        ast::Call(id, call_vars, args) => {
            for (&store_id, arg) in call_vars.iter().zip(args.move_iter()) {
                let rtype = arg.rtype;
                if rtype != ast::AddressType {
                    ops.push_all(trans_expression(ast::Assignment(store_id, box arg.expr)).as_slice());
                }
                else {
                    // Need to special case address types because it is impossible to store them
                    // in memory
                    ops.push_all(trans_expression(arg.expr).as_slice());
                }
            }
            ops.push(UnknownAddr(asm::Call(0), FunctionAddress(id)));
        },
        ast::AsmOperation(op) => {
            ops.push(RawOp(op));
        },
        ast::Jump(id) => {
            ops.push(UnknownAddr(asm::Jump(0), MarkerAddress(id)));
        },
        ast::LitNum(n) => {
            ops.push(RawOp(asm::SetV(0x0, n)));
        },
        ast::Variable(id) => {
            ops.push(UnknownAddr(asm::SetAddress(0), VariableAddress(id)));
            ops.push(RawOp(asm::Read(0x0)));
        },
        ast::Nop => {
            // A NOP compiles to no instructions
        },
    }
    ops
}
