pub mod lower;
pub mod registry;
pub mod resolve;

use registry::DefId;

use crate::{
    ast::{Ident, Span},
    intern::{Symbol, SymbolInterner},
};

use self::registry::{TypeId, VariableId};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Item {
    Def(DefId),
}

/// A resolved type. `span` is where the typepath is
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypePath {
    pub id: TypeId,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionSignature {
    pub name: Ident,
    pub params: Vec<VariableId>,
    pub return_ty: TypePath,
}

// TODO: This should honestly just be an Expr (especially when I make the = syntax for functions)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub exprs: Vec<Expr>,
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

    Loop(Box<Expr>),
    // TODO: Keep track of destination when I add labeled breaks
    Break(Option<Box<Expr>>),
    Continue(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),

    // TODO: Add dot expressions... Need to figure it out in parser
    FnCall(VariableId, Vec<Expr>),
    Block(Block),
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    pub ident: Ident,
    pub type_id: TypeId,
}

pub type Block = Vec<Expr>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal {
    // TODO: I need to rethink how I type literals so that I can have untyped literals like Go
    // where they become whatever type is requested of them or their default type
    pub kind: LiteralKind,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Bool(bool),
    Int(u128),
    Float(Symbol),
    String(Symbol),
    Fn(Fn),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fn {
    pub params: Vec<VariableId>,
    pub return_typepath: TypePath,
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Band,
    Bor,
    Bxor,

    Land,
    Lor,

    Eq,
    Neq,

    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Lnot,
    Bnot,
}
