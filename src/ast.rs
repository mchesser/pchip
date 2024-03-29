use crate::error::InputSpan;

pub use crate::ast::{Expr::*, Item::*, PrimitiveType::*, Type::*};

#[derive(Debug)]
pub struct Program {
    pub items: Vec<Item>,
    pub span: InputSpan,
}

// A top level item
#[derive(Debug)]
pub enum Item {
    FunctionItem(FunctionDeclaration),
    StructItem(StructDeclaration),
    LetItem(LetStatement),
}

impl Item {
    pub fn span(&self) -> InputSpan {
        match self {
            FunctionItem(x) => x.span,
            StructItem(x) => x.span,
            LetItem(x) => x.span,
        }
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    UnitType,
    IntType,
    CharType,
    BoolType,
    AnyType,
    BottomType,
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    Pointer(Box<Type>),
    StaticArrayType(Box<Type>, i32),
    DerefType(Box<Type>),
    FieldRefType(Box<Type>, String),
    UserType(String),
    VariableType(String),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: Box<Expr>,
    pub rtype: Type,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub enum Expr {
    // Control flow
    IfExpr(IfStatement),
    ForLoopExpr(ForLoopStatement),
    LoopExpr(LoopStatement),
    CallExpr(FunctionCall),
    Break,
    Return(Expression),

    // Variables
    LetExpr(LetStatement),
    AssignExpr(Assignment),
    VariableExpr(String),
    StructInitExpr(StructInit),

    LitNumExpr(i32),
    LitCharExpr(char),
    LitStringExpr(String),
    StaticArrayExpr(StaticArray),

    FieldRefExpr(FieldRef),
    ArrayIndexExpr(ArrayIndex),
    RefExpr(Expression),
    DerefExpr(Expression),
    CastExpr(Expression),

    AsmOpExpr(String),
    EmptyExpr,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Expression>,
    pub span: InputSpan,
}

impl Block {
    pub fn rtype(&self) -> Type {
        match self.statements.last() {
            Some(stmt) => stmt.rtype.clone(),
            None => Primitive(UnitType),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Block,
    pub else_block: Option<Block>,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct LoopStatement {
    pub body: Block,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct ForLoopStatement {
    pub loop_var: String,
    pub start: Expression,
    pub end: Expression,
    pub body: Block,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub rtype: Type,
    pub body: Block,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expression>,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub name: String,
    pub var_type: Type,
    pub assignment: Option<Assignment>,
    pub is_const: bool,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub target: Expression,
    pub rhs: Expression,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct StructInit {
    pub type_name: String,
    pub field_init: Vec<(String, Expression)>,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct StaticArray {
    pub elements: Vec<Expression>,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct FieldRef {
    pub field: String,
    pub target: Expression,
    pub span: InputSpan,
}

#[derive(Debug, Clone)]
pub struct ArrayIndex {
    pub index: Expression,
    pub target: Expression,
    pub span: InputSpan,
}
