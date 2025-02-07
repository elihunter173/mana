mod display;
pub mod lex;
pub mod parse;

use crate::intern::Symbol;

// TODO: Should Span be moved?
pub type Span = (usize, usize);

// TODO: Make a Span more generic? Various things can calculate their own span

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Ident {
    pub span: Span,
    pub sym: Symbol,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdentPath {
    pub span: Span,
    pub path: Vec<Ident>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Item {
    Error,
    Def(Def),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Def {
    pub span: Span,
    pub name: Ident,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Error,

    Ident(Ident),
    Literal(LiteralKind),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Access(Box<Expr>, Ident),

    Let(Ident, Option<IdentPath>, Box<Expr>),
    Set(Ident, Box<Expr>),

    Loop(Box<Expr>),
    Break(Option<Box<Expr>>),
    Continue(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),

    FnCall(Ident, Vec<Expr>),
    Block(Vec<Expr>),
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
    },
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
    pub params: Vec<(Ident, IdentPath)>,
    pub return_typepath: Option<IdentPath>,
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
