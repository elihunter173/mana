use core::fmt::{self, Formatter};

use bumpalo::boxed::Box;

use crate::types::Value;

#[derive(Debug)]
pub enum Stmt<'ast> {
    Print(Expr<'ast>),
}

type BoxExpr<'ast> = Box<'ast, Expr<'ast>>;

#[derive(Debug)]
pub enum Expr<'ast> {
    // Literals
    Literal(Value),
    // Expressions
    Typeof(BoxExpr<'ast>),
    Binary(BinOp, BoxExpr<'ast>, BoxExpr<'ast>),
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(val) => write!(f, "{}", val),
            Expr::Binary(l, op, r) => write!(f, "({} {} {})", l, op, r),
            Expr::Typeof(val) => write!(f, "(typeof {})", val),
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Mul,
    Div,
    Add,
    Sub,
    Eq,
    Neq,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BinOp::*;
        f.write_str(match self {
            Mul => "*",
            Div => "/",
            Add => "+",
            Sub => "-",
            Eq => "==",
            Neq => "!=",
        })
    }
}
