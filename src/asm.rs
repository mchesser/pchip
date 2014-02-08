#[allow(dead_code)];
///
/// Description: Convert to bytes
///

use std::vec;
use parser::trans;

pub struct RegId(u8);
pub struct Addr(u16);

/// An operation supported by Chip8
pub enum Operation {
    Clear,
    Return,
    Jump(Addr),
    Call(Addr),
    CompareV(RegId, u8),
    CompareNotV(RegId, u8),
    Compare(RegId, RegId),
    SetV(RegId, u8),
    AddV(RegId, u8),
    Set(RegId, RegId),
    Or(RegId, RegId),
    And(RegId, RegId),
    Xor(RegId, RegId),
    Add(RegId, RegId),
    Sub(RegId, RegId),
    RightShift(RegId, RegId),
    Sub2(RegId, RegId),
    LeftShift(RegId, RegId),
    CompareNot(RegId, RegId),
    SetAddress(Addr),
    Jump2(Addr),
    Random(RegId, u8),
    Draw(RegId, RegId, u8),
    KeyDown(RegId),
    KeyUp(RegId),
    KeyWait(RegId),
    GetDelay(RegId),
    SetDelay(RegId),
    SetSoundDelay(RegId),
    AddAddress(RegId),
    GetFont(RegId),
    BCD(RegId),
    Write(RegId),
    Read(RegId)
}

pub fn compile(code: ~[trans::Operation], num_markers: uint) -> ~[u8] {
    static BASE_ADDRESS: u16 = 0x200;
    let mut actual_ops = ~[];
    let mut markers = vec::from_elem(num_markers, BASE_ADDRESS);
    for operation in code.move_iter() {
        match operation {
            trans::Marker(id) => markers[id] = BASE_ADDRESS + 2 * actual_ops.len() as u16,
            actual_op  => actual_ops.push(actual_op)
        }
    }
    let data_offset = BASE_ADDRESS + 2 * actual_ops.len() as u16;
    let opcodes: ~[u16] = actual_ops.move_iter().map(|operation| {
        match operation {
            trans::RawOp(op) => op,
            trans::UnknownAddr(op, addr) => {
                let raw_addr = match addr {
                    trans::VariableAddress(id) => Addr(data_offset + id as u16),
                    trans::FunctionAddress(_) => fail!("Unimplemented"),
                    trans::MarkerAddress(id) => Addr(markers[id]),
                    trans::RawAddress(address) => Addr(address),
                };
                match op {
                    Jump(_) => Jump(raw_addr),
                    Call(_) => Call(raw_addr),
                    SetAddress(_) => SetAddress(raw_addr),
                    Jump2(_) => Jump2(raw_addr),
                    _ => fail!("`{:?}` does not require an address", op)
                }
            },
            _ => fail!("Unreachable code")
        }
    }).map(|operation| to_opcode(operation)).collect();

    let mut bytes = ~[];
    for &opcode in opcodes.iter() {
        bytes.push((opcode >> 8) as u8);
        bytes.push(opcode as u8);
    }
    bytes
}

fn to_opcode(op: Operation) -> u16 {
    match op {
        Clear                  => 0x00E0,
        Return                 => 0x00EE,
        Jump(addr)             => 0x1000 | addr_opcode(addr),
        Call(addr)             => 0x2000 | addr_opcode(addr),
        CompareV(id, value)    => 0x3000 | reg_value_opcode(id, value),
        CompareNotV(id, value) => 0x4000 | reg_value_opcode(id, value),
        Compare(id1, id2)      => 0x5000 | reg_reg_opcode(id1, id2),
        SetV(id, value)        => 0x6000 | reg_value_opcode(id, value),
        AddV(id, value)        => 0x7000 | reg_value_opcode(id, value),
        Set(id1, id2)          => 0x8000 | reg_reg_opcode(id1, id2),
        Or(id1, id2)           => 0x8001 | reg_reg_opcode(id1, id2),
        And(id1, id2)          => 0x8002 | reg_reg_opcode(id1, id2),
        Xor(id1, id2)          => 0x8003 | reg_reg_opcode(id1, id2),
        Add(id1, id2)          => 0x8004 | reg_reg_opcode(id1, id2),
        Sub(id1, id2)          => 0x8005 | reg_reg_opcode(id1, id2),
        RightShift(id1, id2)   => 0x8006 | reg_reg_opcode(id1, id2),
        Sub2(id1, id2)         => 0x8007 | reg_reg_opcode(id1, id2),
        LeftShift(id1, id2)    => 0x800E | reg_reg_opcode(id1, id2),
        CompareNot(id1, id2)   => 0x9000 | reg_reg_opcode(id1, id2),
        SetAddress(addr)       => 0xA000 | addr_opcode(addr),
        Jump2(addr)            => 0xB000 | addr_opcode(addr),
        Random(id, value)      => 0xC000 | reg_value_opcode(id, value),
        Draw(id1, id2, h)      => 0xD000 | reg_reg_opcode(id1, id2) | h as u16,
        KeyDown(id)            => 0xE09E | reg_opcode(id),
        KeyUp(id)              => 0xE0A1 | reg_opcode(id),
        KeyWait(id)            => 0xF00A | reg_opcode(id),
        GetDelay(id)           => 0xF007 | reg_opcode(id),
        SetDelay(id)           => 0xF015 | reg_opcode(id),
        SetSoundDelay(id)      => 0xF018 | reg_opcode(id),
        AddAddress(id)         => 0xF01E | reg_opcode(id),
        GetFont(id)            => 0xF029 | reg_opcode(id),
        BCD(id)                => 0xF033 | reg_opcode(id),
        Write(id)              => 0xF055 | reg_opcode(id),
        Read(id)               => 0xF065 | reg_opcode(id),
    }
}

fn reg_value_opcode(reg: RegId, value: u8) -> u16 {
    let RegId(id) = reg;
    assert!(id < 0x10);
    ((id as u16) << 8) | (value as u16)
}

fn reg_reg_opcode(reg1: RegId, reg2: RegId) -> u16 {
    let RegId(id1) = reg1;
    let RegId(id2) = reg2;
    assert!(id1 < 0x10 && id2 < 0x10);
    ((id1 as u16) << 8) | ((id2 as u16) << 4)
}

fn reg_opcode(reg: RegId) -> u16 {
    let RegId(id) = reg;
    assert!(id < 0x10);
    ((id as u16) << 8)
}

fn addr_opcode(addr: Addr) -> u16 {
    let Addr(addr) = addr;
    assert!(addr < 0xFFF);
    addr
}
