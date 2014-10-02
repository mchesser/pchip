use std::fmt;

pub type RegId = uint;
pub type SpecialRegId = uint;
pub type TrapId = uint;
pub type LabelId = String;

pub enum Value {
    Const(i16),
    Unknown(LabelId),
}

impl fmt::Show for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Const(val) => write!(f, "{}", val),
            &Unknown(ref label) => write!(f, "{}", label.clone()),
        }
    }
}

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

    RawAsm(String),
}

impl Instruction {
    pub fn into_string(self) -> String {
        match self {
            Load8(j, s, i)   => format!("lb    r{},{}(r{})", j, s, i),
            Load8u(j, s, i)  => format!("lbu   r{},{}(r{})", j, s, i),
            Load16(j, s, i)  => format!("lh    r{},{}(r{})", j, s, i),
            Load16u(j, s, i) => format!("lhu   r{},{}(r{})", j, s, i),
            Load32(j, s, i)  => format!("lw    r{},{}(r{})", j, s, i),
            Store8(s, i, j)  => format!("sb    {}(r{}),r{}", s, i, j),
            Store16(s, i, j) => format!("sh    {}(r{}),r{}", s, i, j),
            Store32(s, i, j) => format!("sw    {}(r{}),r{}", s, i, j),

            JumpIfZero(i, label)    => format!("beqz  r{},{}", i, label),
            JumpIfNotZero(i, label) => format!("bnez  r{},{}", i, label),
            Jump(label)             => format!("j     {}", label),
            JumpStore(label)        => format!("jal   {}", label),
            JumpStoreR(i)           => format!("jalr  r{}", i),
            JumpR(i)                => format!("jr    r{}", i),

            LoadHighImmediate(j, u)   => format!("lhi   r{},{}", j, u),
            AddSigned(k, i, j)        => format!("add   r{},r{},r{}", k, i, j),
            AddSignedValue(j, i, s)   => format!("addi  r{},r{},{}", j, i, s),
            AddUnsigned(k, i, j)      => format!("addu  r{},r{},r{}", k, i, j),
            AddUnsignedValue(j, i, u) => format!("addui r{},r{},{}", j, i, u),
            SubSigned(k, i, j)        => format!("sub   r{},r{},r{}", k, i, j),
            SubSignedValue(j, i, s)   => format!("subi  r{},r{},{}", j, i, s),
            SubUnsigned(k, i, j)      => format!("subu  r{},r{},r{}", k, i, j),
            SubUnsignedValue(j, i, u) => format!("subui r{},r{},{}", j, i, u),

            SetEq(k, i, j)                 => format!("seq   r{},r{},r{}", k, i, j),
            SetEqSignedValue(j, i, s)      => format!("seqi  r{},r{},{}", j, i, s),
            SetEqUnsigned(k, i, j)         => format!("sequ  r{},r{},r{}", k, i, j),
            SetEqUnsignedValue(j, i, u)    => format!("sequi r{},r{},{}", j, i, u),
            SetNotEq(k, i, j)              => format!("sne   r{},r{},r{}", k, i, j),
            SetNotEqSignedValue(j, i, s)   => format!("snei  r{},r{},{}", j, i, s),
            SetNotEqUnsigned(k, i, j)      => format!("sneu  r{},r{},r{}", k, i, j),
            SetNotEqUnsignedValue(j, i, u) => format!("sneui r{},r{},{}", j, i, u),
            SetGt(k, i, j)                 => format!("sgt   r{},r{},r{}", k, i, j),
            SetGtSignedValue(j, i, s)      => format!("sgti  r{},r{},{}", j, i, s),
            SetGtUnsigned(k, i, j)         => format!("sgtu  r{},r{},r{}", k, i, j),
            SetGtUnsignedValue(j, i, u)    => format!("sgtui r{},r{},{}", j, i, u),
            SetGtEq(k, i, j)               => format!("sge   r{},r{},r{}", k, i, j),
            SetGtEqSignedValue(j, i, s)    => format!("sgei  r{},r{},{}", j, i, s),
            SetGtEqUnsigned(k, i, j)       => format!("sgeu  r{},r{},r{}", k, i, j),
            SetGtEqUnsignedValue(j, i, u)  => format!("sgeui r{},r{},{}", j, i, u),
            SetLt(k, i, j)                 => format!("slt   r{},r{},r{}", k, i, j),
            SetLtSignedValue(j, i, s)      => format!("slti  r{},r{},{}", j, i, s),
            SetLtUnsigned(k, i, j)         => format!("sltu  r{},r{},r{}", k, i, j),
            SetLtUnsignedValue(j, i, u)    => format!("sltui r{},r{},{}", j, i, u),
            SetLtEq(k, i, j)               => format!("sle   r{},r{},r{}", k, i, j),
            SetLtEqSignedValue(j, i, s)    => format!("slei  r{},r{},{}", j, i, s),
            SetLtEqUnsigned(k, i, j)       => format!("sleu  r{},r{},r{}", k, i, j),
            SetLtEqUnsignedValue(j, i, u)  => format!("sleui r{},r{},{}", j, i, u),

            And(k, i, j)      => format!("and   r{},r{},r{}", k, i, j),
            AndValue(j, i, u) => format!("andi  r{},r{},{}", j, i, u),
            Or(k, i, j)       => format!("or    r{},r{},r{}", k, i, j),
            OrValue(j, i, u)  => format!("ori   r{},r{},{}", j, i, u),
            Xor(k, i, j)      => format!("xor   r{},r{},r{}", k, i, j),
            XorValue(j, i, u) => format!("xori  r{},r{},{}", j, i, u),

            LShiftValue(j, i, u) => format!("slli  r{},r{},{}", j, i, u),

            Halt => format!("halt"),
            Nop => format!("nop"),

            Label(name) => name,
            AllocateBytes(values) => {
                let mut base = ".byte   ".into_string();
                for &value in values.iter() {
                    base.push_str((format!("{},", value)).as_slice());
                }
                base.pop_char();
                base
            },
            AllocateHalfWords(values) => {
                let mut base = ".half   ".into_string();
                for &value in values.iter() {
                    base.push_str((format!("{},", value)).as_slice());
                }
                base.pop_char();
                base
            },
            AllocateWords(values) => {
                let mut base = ".word   ".into_string();
                for &value in values.iter() {
                    base.push_str((format!("{},", value)).as_slice());
                }
                base.pop_char();
                base
            },
            AllocateSpace(amount) => format!(".space  {}", amount),

            RawAsm(ref data) => data.clone(),
        }
    }
}
