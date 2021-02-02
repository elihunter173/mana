use std::convert::TryInto;

use crate::{
    ast::{BinOp, Expr, Ident, Stmt},
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
            Expr::Ident(_) => {
                panic!("unsupported currently");
            }
            Expr::FnCall(Ident(name), args) => {
                assert!(args.len() == 1);
                let arg = &args[0];
                match *name {
                    "print" => {
                        arg.compile(comp);
                        comp.code.push(OpCode::Print.into());
                    }
                    "typeof" => {
                        arg.compile(comp);
                        comp.code.push(OpCode::Typeof.into());
                    }
                    _ => {
                        panic!("unknown function");
                    }
                }
            }
            Expr::Block { stmts, expr } => {
                for stmt in stmts {
                    stmt.compile(comp);
                }
                if let Some(expr) = expr {
                    expr.compile(comp);
                }
            }
        }
    }
}

impl Compile for Stmt<'_> {
    fn compile(&self, comp: &mut Compiler) {
        match self {
            Stmt::Expr(expr) => {
                expr.compile(comp);
            }
        }
    }
}
