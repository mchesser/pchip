use parser::asm;

pub type MarkerId = uint;
pub type FnId = uint;
pub type VarId = uint;

#[deriving(Show)]
pub struct Marker {
    pub start: MarkerId,
    pub end: MarkerId,
}

#[deriving(Eq)]
pub enum BlockType {
    IfBlock,
    LoopBlock,
    FunctionBlock,
}

#[deriving(Eq, Clone, Show)]
pub enum ReturnType {
    UnitType,
    U8Type,
    AddressType,
    BoolType,
}

#[deriving(Show)]
pub struct Block {
    pub statements: Vec<Expression>,
    pub marker: Marker,
}

impl Block {
    pub fn return_type(&self) -> ReturnType {
        match self.statements.last() {
            Some(ref expr) => expr.rtype,
            None           => UnitType,
        }
    }
}

#[deriving(Show)]
pub struct Expression {
    pub expr: Expr,
    pub rtype: ReturnType,
}

#[deriving(Show)]
pub enum Expr {
    If(~Expr, ~Block, ~Block),
    Loop(~Block),
    Assignment(VarId, ~Expr),
    Call(FnId, Vec<VarId>, Vec<Expression>),
    AsmOperation(asm::Operation),
    Jump(MarkerId),
    LitNum(u8),
    Variable(VarId),
    //OperatorExpr(~Expr, Operator, ~Expr),
    Nop,
}

#[deriving(Show)]
pub struct Function {
    pub arg_types: Vec<ReturnType>,
    pub call_vars: Vec<VarId>,
    pub body: Block,
    pub id: FnId,
}

pub struct Variable {
    pub id: VarId,
}
