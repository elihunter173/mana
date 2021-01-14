use std::convert::TryInto;

use crate::{
    ast::{BinOp, Expr, Stmt},
    bytecode::OpCode,
    types::Value,
};

pub struct Compiler {
    pub code: Vec<u8>,
    pub immediates: Vec<Value>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            immediates: Vec::new(),
        }
    }
}

pub trait Compile {
    fn compile(&self, comp: &mut Compiler);
}

impl Compile for BinOp {
    fn compile(&self, comp: &mut Compiler) {
        let op = match self {
            BinOp::Add => OpCode::Add,
            BinOp::Sub => OpCode::Sub,
            BinOp::Mul => OpCode::Mul,
            BinOp::Div => OpCode::Div,
            BinOp::Eq => OpCode::Eq,
            BinOp::Neq => OpCode::Neq,
        };
        comp.code.push(op.into());
    }
}

impl Compile for Expr<'_> {
    fn compile(&self, comp: &mut Compiler) {
        match self {
            Expr::Literal(value) => {
                let idx = comp.immediates.len().try_into().unwrap();
                comp.immediates.push(*value);
                comp.code.push(OpCode::LoadImd.into());
                comp.code.push(idx);
            }
            Expr::Binary(op, left, right) => {
                left.compile(comp);
                right.compile(comp);
                op.compile(comp);
            }
            Expr::Typeof(expr) => {
                expr.compile(comp);
                comp.code.push(OpCode::Typeof.into());
            }
        }
    }
}

impl Compile for Stmt<'_> {
    fn compile(&self, comp: &mut Compiler) {
        match self {
            Stmt::Print(expr) => {
                expr.compile(comp);
                comp.code.push(OpCode::Print.into());
            }
        }
    }
}
