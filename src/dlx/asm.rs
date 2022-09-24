pub use self::Instruction::*;
pub use self::Value::*;

use std::fmt;

pub type RegId = usize;
pub type _SpecialRegId = usize;
pub type _TrapId = usize;
pub type LabelId = String;

pub enum Value {
    Const(i16),
    _Unknown(LabelId),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Const(val) => write!(f, "{}", val),
            _Unknown(ref label) => write!(f, "{}", label.clone()),
        }
    }
}

#[allow(dead_code)]
pub enum Instruction {
    // Memory transfer instructions
    Load8(RegId, Value, RegId),
    Load8u(RegId, Value, RegId),
    Load16(RegId, Value, RegId),
    Load16u(RegId, Value, RegId),
    Load32(RegId, Value, RegId),
    Store8(Value, RegId, RegId),
    Store16(Value, RegId, RegId),
    Store32(Value, RegId, RegId),

    // Control flow instructions
    JumpIfZero(RegId, LabelId),
    JumpIfNotZero(RegId, LabelId),
    Jump(LabelId),
    JumpStore(LabelId),
    JumpStoreR(RegId),
    JumpR(RegId),

    // Arithmetic instructions
    LoadHighImmediate(RegId, u16),
    AddSigned(RegId, RegId, RegId),
    AddSignedValue(RegId, RegId, i16),
    AddUnsigned(RegId, RegId, RegId),
    AddUnsignedValue(RegId, RegId, u16),
    SubSigned(RegId, RegId, RegId),
    SubSignedValue(RegId, RegId, i16),
    SubUnsigned(RegId, RegId, RegId),
    SubUnsignedValue(RegId, RegId, u16),
    //ClearReg(RegId),
    //NegateReg(RegId),
    //Move(RegId, RegId),
    //MoveSignedValue(RegId, i16),
    //MoveUnsignedValue(RegId, u16),

    // Comparision Instructions
    SetEq(RegId, RegId, RegId),
    SetEqSignedValue(RegId, RegId, i16),
    SetEqUnsigned(RegId, RegId, RegId),
    SetEqUnsignedValue(RegId, RegId, u16),
    SetNotEq(RegId, RegId, RegId),
    SetNotEqSignedValue(RegId, RegId, i16),
    SetNotEqUnsigned(RegId, RegId, RegId),
    SetNotEqUnsignedValue(RegId, RegId, u16),
    SetGt(RegId, RegId, RegId),
    SetGtSignedValue(RegId, RegId, i16),
    SetGtUnsigned(RegId, RegId, RegId),
    SetGtUnsignedValue(RegId, RegId, u16),
    SetGtEq(RegId, RegId, RegId),
    SetGtEqSignedValue(RegId, RegId, i16),
    SetGtEqUnsigned(RegId, RegId, RegId),
    SetGtEqUnsignedValue(RegId, RegId, u16),
    SetLt(RegId, RegId, RegId),
    SetLtSignedValue(RegId, RegId, i16),
    SetLtUnsigned(RegId, RegId, RegId),
    SetLtUnsignedValue(RegId, RegId, u16),
    SetLtEq(RegId, RegId, RegId),
    SetLtEqSignedValue(RegId, RegId, i16),
    SetLtEqUnsigned(RegId, RegId, RegId),
    SetLtEqUnsignedValue(RegId, RegId, u16),

    // Bit manipulation instructions
    And(RegId, RegId, RegId),
    AndValue(RegId, RegId, u16),
    Or(RegId, RegId, RegId),
    OrValue(RegId, RegId, u16),
    Xor(RegId, RegId, RegId),
    XorValue(RegId, RegId, u16),

    // LShift(RegId, RegId, RegId),
    LShiftValue(RegId, RegId, u16),
    // RShiftSign(RegId, RegId, RegId),
    // RShiftSignValue(RegId, RegId, u16),
    // RShiftZero(RegId, RegId, RegId),
    // RShiftZeroValue(RegId, RegId, u16),
    // LShiftSign(RegId, RegId, RegId),
    // LShiftSignValue(RegId, RegId, u16),
    // LShiftZero(RegId, RegId, RegId),
    // LShiftZeroValue(RegId, RegId, u16),

    // Special Instructions
    Halt,
    // StoreSpecial(SpecialRegId, RegId),
    // LoadSpecial(RegId, SpecialRegId),
    Nop,
    // Trap(TrapId),
    // ReturnFromException,

    // Assembler Directives
    Label(LabelId),
    AllocateBytes(Vec<i8>),
    AllocateHalfWords(Vec<i16>),
    AllocateWords(Vec<i32>),
    AllocateSpace(u32),
    AllocateAscii(String),
    Align(i16),

    RawAsm(String),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Load8(j, s, i) => write!(f, "lb      r{},{}(r{})", j, s, i),
            Load8u(j, s, i) => write!(f, "lbu     r{},{}(r{})", j, s, i),
            Load16(j, s, i) => write!(f, "lh      r{},{}(r{})", j, s, i),
            Load16u(j, s, i) => write!(f, "lhu     r{},{}(r{})", j, s, i),
            Load32(j, s, i) => write!(f, "lw      r{},{}(r{})", j, s, i),
            Store8(s, i, j) => write!(f, "sb      {}(r{}),r{}", s, i, j),
            Store16(s, i, j) => write!(f, "sh      {}(r{}),r{}", s, i, j),
            Store32(s, i, j) => write!(f, "sw      {}(r{}),r{}", s, i, j),

            JumpIfZero(i, label) => write!(f, "beqz    r{},{}", i, label),
            JumpIfNotZero(i, label) => write!(f, "bnez    r{},{}", i, label),
            Jump(label) => write!(f, "j       {}", label),
            JumpStore(label) => write!(f, "jal     {}", label),
            JumpStoreR(i) => write!(f, "jalr    r{}", i),
            JumpR(i) => write!(f, "jr      r{}", i),

            LoadHighImmediate(j, u) => write!(f, "lhi     r{},{}", j, u),
            AddSigned(k, i, j) => write!(f, "add     r{},r{},r{}", k, i, j),
            AddSignedValue(j, i, s) => write!(f, "addi    r{},r{},{}", j, i, s),
            AddUnsigned(k, i, j) => write!(f, "addu    r{},r{},r{}", k, i, j),
            AddUnsignedValue(j, i, u) => write!(f, "addui   r{},r{},{}", j, i, u),
            SubSigned(k, i, j) => write!(f, "sub     r{},r{},r{}", k, i, j),
            SubSignedValue(j, i, s) => write!(f, "subi    r{},r{},{}", j, i, s),
            SubUnsigned(k, i, j) => write!(f, "subu    r{},r{},r{}", k, i, j),
            SubUnsignedValue(j, i, u) => write!(f, "subui   r{},r{},{}", j, i, u),

            SetEq(k, i, j) => write!(f, "seq     r{},r{},r{}", k, i, j),
            SetEqSignedValue(j, i, s) => write!(f, "seqi    r{},r{},{}", j, i, s),
            SetEqUnsigned(k, i, j) => write!(f, "sequ    r{},r{},r{}", k, i, j),
            SetEqUnsignedValue(j, i, u) => write!(f, "sequi   r{},r{},{}", j, i, u),
            SetNotEq(k, i, j) => write!(f, "sne     r{},r{},r{}", k, i, j),
            SetNotEqSignedValue(j, i, s) => write!(f, "snei    r{},r{},{}", j, i, s),
            SetNotEqUnsigned(k, i, j) => write!(f, "sneu    r{},r{},r{}", k, i, j),
            SetNotEqUnsignedValue(j, i, u) => write!(f, "sneui   r{},r{},{}", j, i, u),
            SetGt(k, i, j) => write!(f, "sgt     r{},r{},r{}", k, i, j),
            SetGtSignedValue(j, i, s) => write!(f, "sgti    r{},r{},{}", j, i, s),
            SetGtUnsigned(k, i, j) => write!(f, "sgtu    r{},r{},r{}", k, i, j),
            SetGtUnsignedValue(j, i, u) => write!(f, "sgtui   r{},r{},{}", j, i, u),
            SetGtEq(k, i, j) => write!(f, "sge     r{},r{},r{}", k, i, j),
            SetGtEqSignedValue(j, i, s) => write!(f, "sgei    r{},r{},{}", j, i, s),
            SetGtEqUnsigned(k, i, j) => write!(f, "sgeu    r{},r{},r{}", k, i, j),
            SetGtEqUnsignedValue(j, i, u) => write!(f, "sgeui   r{},r{},{}", j, i, u),
            SetLt(k, i, j) => write!(f, "slt     r{},r{},r{}", k, i, j),
            SetLtSignedValue(j, i, s) => write!(f, "slti    r{},r{},{}", j, i, s),
            SetLtUnsigned(k, i, j) => write!(f, "sltu    r{},r{},r{}", k, i, j),
            SetLtUnsignedValue(j, i, u) => write!(f, "sltui   r{},r{},{}", j, i, u),
            SetLtEq(k, i, j) => write!(f, "sle     r{},r{},r{}", k, i, j),
            SetLtEqSignedValue(j, i, s) => write!(f, "slei    r{},r{},{}", j, i, s),
            SetLtEqUnsigned(k, i, j) => write!(f, "sleu    r{},r{},r{}", k, i, j),
            SetLtEqUnsignedValue(j, i, u) => write!(f, "sleui   r{},r{},{}", j, i, u),

            And(k, i, j) => write!(f, "and     r{},r{},r{}", k, i, j),
            AndValue(j, i, u) => write!(f, "andi    r{},r{},{}", j, i, u),
            Or(k, i, j) => write!(f, "or      r{},r{},r{}", k, i, j),
            OrValue(j, i, u) => write!(f, "ori     r{},r{},{}", j, i, u),
            Xor(k, i, j) => write!(f, "xor     r{},r{},r{}", k, i, j),
            XorValue(j, i, u) => write!(f, "xori    r{},r{},{}", j, i, u),

            LShiftValue(j, i, u) => write!(f, "slli    r{},r{},{}", j, i, u),

            Halt => f.write_str("halt"),
            Nop => f.write_str("nop"),

            Label(name) => f.write_str(name),
            AllocateBytes(values) => {
                let mut base = ".byte   ".to_string();
                for &value in values {
                    base.push_str(&format!("{},", value));
                }
                base.pop();
                f.write_str(&base)
            }
            AllocateHalfWords(values) => {
                let mut base = ".half   ".to_string();
                for &value in values {
                    base.push_str(&format!("{},", value));
                }
                base.pop();
                f.write_str(&base)
            }
            AllocateWords(values) => {
                let mut base = ".word   ".to_string();
                for &value in values {
                    base.push_str(&format!("{},", value));
                }
                base.pop();
                f.write_str(&base)
            }
            AllocateSpace(amount) => write!(f, "   .space  {}", amount),
            AllocateAscii(value) => write!(f, "   .ascii  \"{}\"", value),
            Align(value) => write!(f, "   .align  {}", value),

            RawAsm(data) => f.write_str(data),
        }
    }
}
