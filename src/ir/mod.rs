pub mod lower;
pub mod registry;
pub mod resolve;

use crate::{
    ast::{Ident, Span},
    intern::{Symbol, SymbolInterner},
};

use self::{
    registry::{FunctionId, TypeId, VariableId},
    resolve::ObjectId,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

// TODO: Replace Types with object ids probably

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Item {
    // TODO: Holy fuck what is the ownership situation of function definitions
    Function(FunctionId),
    // TODO: Add imports
}

/// A resolved type. `span` is where the typepath is
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub id: TypeId,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<(Ident, Type)>,
    pub return_ty: Type,
    pub body: Vec<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Variable(VariableId),
    Literal(Literal),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),

    Let(VariableId, Box<Expr>),
    Set(VariableId, Box<Expr>),

    // TODO: Add dot expressions... Need to figure it out in parser
    // TODO: This shouldn't be an ObjectId I don't think
    FnCall(ObjectId, Vec<Expr>),
    Block(Block),
    If {
        cond: Box<Expr>,
        // TODO: Maybe make this a Block type?
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    pub span: Span,
    pub ty: TypeId,
}

pub type Block = Vec<Expr>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal {
    pub span: Span,
    // TODO: I need to rethink this so I can more easily convert to a type
    pub kind: LiteralKind,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Bool(bool),
    Int(u128),
    Float(Symbol),
    String(Symbol),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Mul,
    Div,
    Add,
    Sub,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Land,
    Lor,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}
