///
/// Description: Translation to chip8 instructions
///

use parser::asm;
use parser::asm::{RegId, Addr};

pub type VarId = uint;
pub type MarkerId = uint;
pub type FuncId = uint;

pub enum Address {
    VariableAddress(VarId),
    FunctionAddress(FuncId),
    MarkerAddress(MarkerId),
    RawAddress(u16)
}

pub enum Operation {
    RawOp(asm::Operation),
    UnknownAddr(asm::Operation, Address),
    Marker(MarkerId),
}

pub enum Operator {
    Plus,
    Minus,
    Or,
    Xor
}

pub struct Block {
    statements: ~[Statement],
    start_id: MarkerId,
    end_id: MarkerId,
}

pub fn block_to_asm(block: Block) -> ~[Operation] {
    let start_id = block.start_id;
    let end_id = block.end_id;
    let mut acc = ~[Marker(start_id)];
    for statement in block.statements.move_iter() {
        acc.push_all_move(statement_to_asm(statement));
    }
    acc.push(Marker(end_id));
    acc
}

pub enum Statement {
    If(Expression, Block, Block),
    Loop(Block),
    Assignment(VarId, Expression),
    CallSys(SysCall),
    Jump(MarkerId),
    SetAddress(Address),
    Nop,
}

fn statement_to_asm(statement: Statement) -> ~[Operation] {
    match statement {
        If(expr, then_block, else_block) => {
            // If
            let mut ops = expression_to_asm(expr);
            ops.push(RawOp(asm::CompareV(RegId(0x0), 0x0)));
            ops.push(UnknownAddr(asm::Jump(Addr(0)), MarkerAddress(else_block.start_id)));
            // Then
            ops.push_all_move(block_to_asm(then_block));
            ops.push(UnknownAddr(asm::Jump(Addr(0)), MarkerAddress(else_block.end_id)));
            // Else
            ops.push_all_move(block_to_asm(else_block));

            ops
        },
        Loop(block) => {
            block_to_asm(block)
        },
        Assignment(var, expr) => {
            let mut ops = expression_to_asm(expr);
            ops.push(UnknownAddr(asm::SetAddress(Addr(0)), VariableAddress(var)));
            ops.push(RawOp(asm::Write(RegId(0x0))));
            ops
        },
        CallSys(call) => {
            syscall_to_asm(call)
        },
        Jump(id) => {
            ~[UnknownAddr(asm::Jump(Addr(0)), MarkerAddress(id))]
        },
        SetAddress(addr) => {
            ~[UnknownAddr(asm::SetAddress(Addr(0)), addr)]
        }
        Nop => {
            ~[]
        }
    }
}

pub enum SysCall {
    Clear,
    DrawPos(Expression, Expression),
    SetAddr(Address),
    Draw(u8),
    Random(Expression, u8),
    SetDelay(Expression),
    SetSound(Expression),
    GetFont(Expression),
    KeyWait(Expression),
}

fn syscall_to_asm(syscall: SysCall) -> ~[Operation] {
    match syscall {
        Clear => {
            ~[RawOp(asm::Clear)]
        },
        DrawPos(expr1, expr2) => {
            let mut ops = expression_to_asm(expr1);
            ops.push(RawOp(asm::Set(RegId(0xE), RegId(0x0))));
            ops.push_all_move(expression_to_asm(expr2));
            ops.push(RawOp(asm::Set(RegId(0xD), RegId(0x0))));
            ops
        },
        SetAddr(addr) => {
            ~[UnknownAddr(asm::SetAddress(Addr(0)), addr)]
        },
        Draw(h) => {
            ~[RawOp(asm::Draw(RegId(0xE), RegId(0xD), h))]

        },
        Random(expr, mask) => {
            let mut ops = expression_to_asm(expr);
            ops.push(RawOp(asm::Random(RegId(0x0), mask)));
            ops
        },
        SetDelay(expr) => {
            let mut ops = expression_to_asm(expr);
            ops.push(RawOp(asm::SetDelay(RegId(0x0))));
            ops
        },
        SetSound(expr) => {
            let mut ops = expression_to_asm(expr);
            ops.push(RawOp(asm::SetSoundDelay(RegId(0x0))));
            ops
        },
        GetFont(expr) => {
            let mut ops = expression_to_asm(expr);
            ops.push(RawOp(asm::GetFont(RegId(0x0))));
            ops
        },
        KeyWait(expr) => {
            let mut ops = expression_to_asm(expr);
            ops.push(RawOp(asm::KeyWait(RegId(0x0))));
            ops
        }
    }
}

pub enum Expression {
    LitNum(u8),
    Variable(VarId),
    OperatorExpr(~Expression, Operator, ~Expression)
}

fn expression_to_asm(expr: Expression) -> ~[Operation] {
    match expr {
        LitNum(n) => {
            ~[RawOp(asm::SetV(RegId(0x0), n))]
        },
        Variable(id) => {
            ~[
                UnknownAddr(asm::SetAddress(Addr(0)), VariableAddress(id)),
                RawOp(asm::Read(RegId(0x0)))
            ]
        }
        OperatorExpr(~e1, operator, ~e2) => {
            let mut ops = expression_to_asm(e1);
            ops.push(RawOp(asm::Set(RegId(0x1), RegId(0x0))));
            ops.push_all_move(expression_to_asm(e2));
            match operator {
                Plus => {
                    ops.push(RawOp(asm::Add(RegId(0x0), RegId(0x1))));
                },
                Minus => {
                    ops.push(RawOp(asm::Sub(RegId(0x0), RegId(0x1))));
                },
                Or => {
                    ops.push(RawOp(asm::Or(RegId(0x0), RegId(0x1))));
                },
                Xor => {
                    ops.push(RawOp(asm::Xor(RegId(0x0), RegId(0x1))));
                },
            }
            ops
        }
    }
}