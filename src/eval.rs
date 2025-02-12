use crate::{
    intern::SymbolInterner,
    ir::{
        self,
        registry::{FunctionId, Registry},
        *,
    },
};

pub struct Machine<'ctx> {
    module: ir::Module,

    symbols: &'ctx SymbolInterner,
    registry: &'ctx Registry,
}

#[derive(Debug)]
enum EvalResult {
    Value(Val),
    Break(Val),
    Return(Val),
}

macro_rules! flow {
    ($v:expr) => {
        match $v {
            EvalResult::Value(v) => v,
            v => return v,
        }
    };
}

impl<'ctx> Machine<'ctx> {
    pub fn new(
        module: ir::Module,
        symbols: &'ctx SymbolInterner,
        registry: &'ctx Registry,
    ) -> Self {
        Self { module, symbols, registry }
    }

    pub fn start(&mut self) {
        let main = self.symbols.get("main").expect("main must be defined");

        let main = self
            .module
            .items
            .iter()
            .find_map(|item| match item {
                ir::Item::Def(def_id) => {
                    let (name, expr) = self.registry.get_def(*def_id);
                    if name.sym == main {
                        Some(expr.expect("main must be defined"))
                    } else {
                        None
                    }
                }
            })
            .expect("main must be defined");

        println!("main returns {:?}", self.eval(main));
    }

    pub fn eval(&mut self, expr: &ir::Expr) -> EvalResult {
        match &expr.kind {
            ir::ExprKind::Variable(variable_id) => todo!(),
            ir::ExprKind::Literal(literal) => EvalResult::Value(val_from_literal(literal)),

            ir::ExprKind::Binary(bin_op, left, right) => {
                let left_val = flow!(self.eval(left));
                let right_val = flow!(self.eval(right));
                let val = self.eval_binary(*bin_op, left_val, right_val);
                EvalResult::Value(val)
            }

            ir::ExprKind::Unary(unary_op, expr) => {
                let pre_val = flow!(self.eval(expr));
                let post_val = self.eval_unary(*unary_op, pre_val);
                EvalResult::Value(post_val)
            }

            ir::ExprKind::Let(variable_id, expr) => todo!(),
            ir::ExprKind::Set(variable_id, expr) => todo!(),
            ir::ExprKind::Loop(expr) => todo!(),
            ir::ExprKind::Break(expr) => todo!(),
            ir::ExprKind::Continue => todo!(),
            ir::ExprKind::Return(expr) => {
                let val = if let Some(expr) = expr {
                    flow!(self.eval(expr))
                } else {
                    Val::Unit
                };
                EvalResult::Return(val)
            }

            ir::ExprKind::FnCall { callee, args } => match flow!(self.eval(callee)) {
                Val::Func(function_id) => todo!("register variables"),
                _ => panic!("type error"),
            },
            ir::ExprKind::Block(exprs) => todo!(),
            ir::ExprKind::If { cond, then_expr, else_expr } => match flow!(self.eval(cond)) {
                Val::Bool(true) => self.eval(&then_expr),
                Val::Bool(false) => {
                    if let Some(else_expr) = else_expr {
                        self.eval(&else_expr)
                    } else {
                        EvalResult::Value(Val::Unit)
                    }
                }
                _ => panic!("type error"),
            },
        }
    }

    fn eval_binary(&mut self, bin_op: BinOp, left: Val, right: Val) -> Val {
        match bin_op {
            BinOp::Add => match (left, right) {
                _ => panic!("type error"),
            },

            BinOp::Sub => match (left, right) {
                _ => panic!("type error"),
            },

            BinOp::Mul => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Div => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Rem => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Band => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Bor => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Bxor => match (left, right) {
                _ => panic!("type error"),
            },

            BinOp::Land => match (left, right) {
                (Val::Bool(l), Val::Bool(r)) => Val::Bool(l == r),
                _ => panic!("type safety"),
            },
            BinOp::Lor => match (left, right) {
                (Val::Bool(l), Val::Bool(r)) => Val::Bool(l || r),
                _ => panic!("type error"),
            },

            BinOp::Eq => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Neq => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Lt => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Leq => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Gt => match (left, right) {
                _ => panic!("type error"),
            },
            BinOp::Geq => match (left, right) {
                _ => panic!("type error"),
            },
        }
    }

    fn eval_unary(&mut self, unary_op: UnaryOp, val: Val) -> Val {
        match unary_op {
            UnaryOp::Neg => match val {
                Val::Int8(v) => Val::Int8(-v),
                Val::Int16(v) => Val::Int16(-v),
                Val::Int32(v) => Val::Int32(-v),
                Val::Int64(v) => Val::Int64(-v),

                Val::Float32(v) => Val::Float32(-v),
                Val::Float64(v) => Val::Float64(-v),

                _ => panic!("type error"),
            },
            UnaryOp::Lnot => match val {
                Val::Bool(v) => Val::Bool(!v),
                _ => panic!("type error"),
            },
            UnaryOp::Bnot => todo!(),
        }
    }
}

fn val_from_literal(literal: &Literal) -> Val {
    match literal.kind {
        LiteralKind::Bool(_) => todo!(),
        LiteralKind::Int(v) => Val::Int32(v as _),
        LiteralKind::Float(symbol) => todo!(),
        LiteralKind::Fn(_) => todo!("these should already be handled by this point"),
    }
}

/// Mana value
#[derive(Debug)]
enum Val {
    Unit,
    Bool(bool),
    // uints
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    // ints
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    // floats
    Float32(f32),
    Float64(f64),
    // others
    Func(FunctionId),
}
