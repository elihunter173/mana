use std::str::FromStr;

use num::{BigInt, Zero};

// TODO: I think Literal should probably just be the string?
#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool),
    Int(BigInt),
    Float(f64),
    String(String),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(s), Self::Bool(o)) => s == o,
            (Self::Int(s), Self::Int(o)) => s == o,
            (Self::String(s), Self::String(o)) => s == o,
            (Self::Float(s), Self::Float(o)) => (s.is_nan() && o.is_nan()) || s == o,
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Literal {
    pub fn parse_int(digits: &str, radix: u32) -> Self {
        // TODO: Maybe optimize this?
        let mut num = BigInt::zero();
        for b in digits.bytes() {
            let d = match b {
                b'0'..=b'9' => b - b'0',
                b'a'..=b'z' => b - b'a' + 10,
                b'A'..=b'Z' => b - b'A' + 10,
                b'_' => continue,
                _ => panic!("unexpected digit {:?}", b),
            };
            num *= radix;
            num += d;
        }

        Self::Int(num)
    }

    pub fn parse_float(input: &str) -> Self {
        // TODO: Use better parsing logic
        let input = input.replace("_", "");
        Literal::Float(f64::from_str(&input).unwrap())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
// TODO: Why do we own the string
pub struct Ident(pub String);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypePath {
    pub path: Vec<Ident>,
}

// Is this a good way to do things?
// pub struct Expr {
//   typ: Type,
//   kind: ExprKind,
// }

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item {
    FnDef {
        name: Ident,
        args: Vec<(Ident, TypePath)>,
        return_type: Option<TypePath>,
        body: Vec<Expr>,
    },
    Import(TypePath),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),

    Let(Ident, Box<Expr>),
    Set(Ident, Box<Expr>),

    FnCall(Ident, Vec<Expr>),
    Block(Vec<Expr>),
    If {
        cond: Box<Expr>,
        true_expr: Box<Expr>,
        false_expr: Option<Box<Expr>>,
    },
}

impl Expr {
    // TODO: Find a way to easily iterate over the children
    pub fn apply_children<F>(&self, mut f: F)
    where
        F: FnMut(&Expr),
    {
        use Expr::*;
        match self {
            Ident(_) | Literal(_) => {}
            Binary(_, l, r) => {
                f(l);
                f(r);
            }
            Unary(_, x) | Let(_, x) | Set(_, x) => {
                f(x);
            }
            If { cond: _, true_expr, false_expr } => {
                f(true_expr);
                if let Some(false_expr) = false_expr {
                    f(false_expr);
                }
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Neg,
}
