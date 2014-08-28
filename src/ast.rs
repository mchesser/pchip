use error::InputSpan;

#[deriving(Show)]
pub struct Program {
    pub items: Vec<Item>,
    pub span: InputSpan,
}

// A top level item
#[deriving(Show)]
pub enum Item {
    FunctionItem(FunctionDeclaration),
    StructItem(StructDeclaration),
    LetItem(LetStatement),
}

impl Item {
    pub fn span(&self) -> InputSpan {
        match self {
            &FunctionItem(ref x) => x.span.clone(),
            &StructItem(ref x) => x.span.clone(),
            &LetItem(ref x) => x.span.clone(),
        }
    }
}

#[deriving(Show, Hash, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    UnitType,
    IntType,
    BoolType,
    // AnyType,
    BottomType,
}

impl PrimitiveType {
    pub fn size(&self) -> uint {
        match *self {
            UnitType => 0,
            IntType => 4,
            BoolType => 4,
            BottomType => 0,
        }
    }
}

#[deriving(Show, Hash, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    UserType(String),
    VariableType(String),
}

#[deriving(Show, Clone)]
pub struct Expression {
    pub expr: Box<Expr>,
    pub rtype: Type,
}

#[deriving(Show, Clone)]
pub enum Expr {
    // Control flow
    IfExpr(IfStatement),
    LoopExpr(LoopStatement),
    CallExpr(FunctionCall),
    Break,

    // Variables
    LetExpr(LetStatement),
    AssignExpr(Assignment),
    VariableExpr(String),
    LitNumExpr(int),

    // Other
    AsmOpExpr(String),
    EmptyExpr,
}

#[deriving(Show, Clone)]
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

#[deriving(Show, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Block,
    pub else_block: Option<Block>,
    pub span: InputSpan,
}

#[deriving(Show, Clone)]
pub struct LoopStatement {
    pub body: Block,
    pub span: InputSpan,
}

#[deriving(Show, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub components: Vec<(String, PrimitiveType)>,
    pub span: InputSpan,
}

#[deriving(Show, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub rtype: Type,
    pub body: Block,
    pub span: InputSpan,
}

#[deriving(Show, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expression>,
    pub span: InputSpan,
}

#[deriving(Show, Clone)]
pub struct LetStatement {
    pub name: String,
    pub var_type: Type,
    pub assignment: Option<Assignment>,
    pub span: InputSpan,
}

#[deriving(Show, Clone)]
pub struct Assignment {
    pub target: String,
    pub expression: Expression,
    pub span: InputSpan,
}
