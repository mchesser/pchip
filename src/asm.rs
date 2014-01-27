/// 
/// Description: Methods for converting input from trans to CHIP8 code
///

struct RegId(u8);
struct Addr(u16)

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
    KeyUp(RegId)
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
        Draw(id1, id2, h)      => 0xD000 | reg_reg_opcode(id1, id2) | h,
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
    assert!(reg < 0x10);
    ((id as u16) << 8) | (value as u16)
}

fn reg_reg_opcode(reg1: RegId, reg2: RegId) -> u16 {
    let RegId(id1) = reg1;
    let RegId(id2) = reg2;
    assert!(reg1 < 0x10 && reg2 < 0x10);
    ((id1 as u16) << 8) | ((id2 as u16) << 4)
}

fn reg_opcode(reg: RegId) -> u16 {
    let RegId(id) = reg;
    assert!(reg < 0x10);
    ((id1 as u16) << 8)
}

fn addr_opcode(addr: Addr) -> u16 {
    let Addr(addr) = addr;
    assert!(addr < 0xFFF);
    addr
}


