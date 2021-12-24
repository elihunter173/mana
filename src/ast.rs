use crate::intern::Symbol;

pub type Span = (usize, usize);

// TODO: Make a HasSpan trait? Typepath can calculate its own span
// TODO: Maybe I should create a Spanned helper struct?

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub kind: T,
}

pub type Literal = Spanned<LiteralKind>;

// TODO: Should I make this more specific (i.e. have Int, IntHex, ...) or more general?
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Bool(bool),
    Int(u128),
    // TODO: Maybe use a symbol?
    Float(Symbol),
    String(Symbol),
}

// TODO: Move this to a different module?
// TODO: Maybe use Spanned?
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Ident {
    pub span: Span,
    pub name: Symbol,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypePath {
    pub span: Span,
    pub path: Vec<Ident>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnDef {
    pub name: Ident,
    pub params: Vec<(Ident, TypePath)>,
    pub return_typepath: Option<TypePath>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Item {
    FnDef(FnDef),
    Import(TypePath),
}

pub type Expr = Spanned<ExprKind>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Ident(Ident),
    Literal(Literal),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),

    Let(Ident, Option<TypePath>, Box<Expr>),
    Set(Ident, Box<Expr>),

    FnCall(Ident, Vec<Expr>),
    Block(Block),
    // TODO: Make this a struct?
    If {
        cond: Box<Expr>,
        // TODO: Maybe make this a Block type?
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
