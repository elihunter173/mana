use std::str::FromStr;

pub type Span = (usize, usize);

// TODO: Make a HasSpan trait? Typepath can calculate its own span
// TODO: Maybe I should create a Spanned helper struct?

#[derive(Clone, Debug)]
pub struct Literal {
    pub span: Span,
    pub kind: LiteralKind,
}

// TODO: I think Literal should probably just be the string?
#[derive(Clone, Debug)]
pub enum LiteralKind {
    Bool(bool),
    // TODO: This should be better
    Int(isize),
    Float(f64),
    String(String),
}

impl LiteralKind {
    pub fn parse_int(digits: &str, radix: u32) -> Self {
        // TODO: Maybe optimize this?
        let mut num = 0;
        for b in digits.bytes() {
            let d = match b {
                b'0'..=b'9' => b - b'0',
                b'a'..=b'z' => b - b'a' + 10,
                b'A'..=b'Z' => b - b'A' + 10,
                b'_' => continue,
                _ => panic!("unexpected digit {:?}", b),
            };
            num *= radix as isize;
            num += d as isize;
        }

        Self::Int(num)
    }

    pub fn parse_float(input: &str) -> Self {
        // TODO: Use better parsing logic
        let input = input.replace("_", "");
        Self::Float(f64::from_str(&input).unwrap())
    }
}

// TODO: Why do we own the string
// TODO: Add a span/
#[derive(Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct TypePath {
    pub span: Span,
    pub path: Vec<Ident>,
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub name: Ident,
    pub params: Vec<(Ident, TypePath)>,
    pub return_typepath: Option<TypePath>,
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum Item {
    FnDef(FnDef),
    Import(TypePath),
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Ident(Ident),
    Literal(Literal),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),

    Let(Ident, Option<TypePath>, Box<Expr>),
    Set(Ident, Box<Expr>),

    FnCall(Ident, Vec<Expr>),
    Block(Vec<Expr>),
    If {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Option<Box<Expr>>,
    },
}

impl Expr {
    // TODO: Find a way to easily iterate over the children
    pub fn apply_children<F>(&self, mut f: F)
    where
        F: FnMut(&Expr),
    {
        match &self.kind {
            ExprKind::Ident(_) | ExprKind::Literal(_) => {}
            ExprKind::Binary(_, l, r) => {
                f(l);
                f(r);
            }
            ExprKind::Unary(_, x) | ExprKind::Let(_, _, x) | ExprKind::Set(_, x) => {
                f(x);
            }
            ExprKind::If {
                cond: _,
                then_expr: true_expr,
                else_expr: false_expr,
            } => {
                f(true_expr);
                if let Some(false_expr) = false_expr {
                    f(false_expr);
                }
            }
            ExprKind::Block(exprs) => {
                exprs.iter().for_each(f);
            }
            // TODO: Finish
            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Neg,
}
