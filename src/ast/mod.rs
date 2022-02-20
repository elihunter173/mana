use crate::intern::Symbol;

pub mod lex;
pub mod parse;

// TODO: Should Span be moved?
// TODO: Make a HasSpan trait? Various things can calculate their own span

pub type Span = (usize, usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub kind: T,
}

pub type Literal = Spanned<LiteralKind>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Item {
    FnDef(FnDef),
    Import(IdentPath),
}

// TODO: Should I make this more specific (i.e. have Int, IntHex, ...) or more general?
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Bool(bool),
    Int(u128),
    Float(Symbol),
    String(Symbol),
}

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
pub struct FnDef {
    pub name: Ident,
    pub params: Vec<(Ident, IdentPath)>,
    pub return_typepath: Option<IdentPath>,
    pub body: Block,
    pub span: Span,
}

pub type Expr = Spanned<ExprKind>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Ident(Ident),
    Literal(Literal),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),

    Let(Ident, Option<IdentPath>, Box<Expr>),
    Set(Ident, Box<Expr>),

    FnCall(Ident, Vec<Expr>),
    Block(Block),
    // TODO: Make this a struct?
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
    },
}

// TODO: Maybe make this a full blown new type?
pub type Block = Vec<Expr>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Mul,
    Div,
    Add,
    Sub,
    Eq,
    Neq,
    Land,
    Lor,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}
