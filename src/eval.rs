use std::iter;

use crate::{
    intern::SymbolInterner,
    ir::{
        self,
        registry::{Registry, VariableId},
        *,
    },
    resolve::Resolver,
};

pub struct Machine<'ctx> {
    module: ir::Module,

    variables: Resolver<VariableId, Val>,
    symbols: &'ctx SymbolInterner,
    registry: &'ctx Registry,
}

#[derive(Debug)]
enum EvalResult {
    Value(Val),
    Continue,
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
        Self {
            module,
            variables: Resolver::new(),
            symbols,
            registry,
        }
    }

    pub fn main(&mut self) -> Val {
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

        // TODO: I should be passing already evaluated IR here
        let ExprKind::Literal(Literal { kind: LiteralKind::Fn(ref main), .. }) = main.kind else {
            panic!("main must be a function");
        };

        // TODO: Call the function more easily
        self.eval_fn_call(main.clone(), vec![])
    }

    pub fn eval(&mut self, expr: &ir::Expr) -> EvalResult {
        match &expr.kind {
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

            ir::ExprKind::Let(variable_id, expr) => {
                let val = flow!(self.eval(expr));
                if self.variables.define(*variable_id, val).is_some() {
                    panic!("overriding variable in let");
                }
                EvalResult::Value(Val::Unit)
            }
            ir::ExprKind::Set(variable_id, expr) => {
                let val = flow!(self.eval(expr));
                // TODO: This doesn't work if the variable is in a different level. Gahhhhh
                // I think it's a bad idea that I had the resolver be generic and shared :dead:
                if self.variables.define(*variable_id, val).is_none() {
                    panic!("defining variable in set");
                }
                EvalResult::Value(Val::Unit)
            }
            ir::ExprKind::Variable(variable_id) => EvalResult::Value(
                self.variables
                    .resolve(variable_id)
                    .expect("variable defined")
                    .clone(),
            ),

            ir::ExprKind::Loop(expr) => loop {
                match self.eval(expr) {
                    EvalResult::Value(_) => continue,
                    EvalResult::Continue => continue,
                    EvalResult::Break(val) => break EvalResult::Value(val),
                    EvalResult::Return(val) => break EvalResult::Return(val),
                }
            },
            ir::ExprKind::Break(expr) => {
                if let Some(expr) = expr {
                    EvalResult::Break(flow!(self.eval(expr)))
                } else {
                    EvalResult::Break(Val::Unit)
                }
            }
            ir::ExprKind::Continue => EvalResult::Continue,
            ir::ExprKind::Return(expr) => {
                let val = if let Some(expr) = expr {
                    flow!(self.eval(expr))
                } else {
                    Val::Unit
                };
                EvalResult::Return(val)
            }

            ir::ExprKind::FnCall { callee, args } => match flow!(self.eval(callee)) {
                Val::Func(func) => {
                    let mut arg_vals = Vec::with_capacity(args.len());
                    for arg in args {
                        let val = flow!(self.eval(arg));
                        arg_vals.push(val);
                    }
                    EvalResult::Value(self.eval_fn_call(func, arg_vals))
                }
                _ => panic!("type error"),
            },
            ir::ExprKind::Block(exprs) => {
                self.variables.enter_scope();
                let mut last = Val::Unit;
                for expr in exprs {
                    last = flow!(self.eval(expr));
                }
                self.variables.exit_scope();
                EvalResult::Value(last)
            }
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
                (Val::UInt8(l), Val::UInt8(r)) => Val::UInt8(l.wrapping_add(r)),
                (Val::UInt16(l), Val::UInt16(r)) => Val::UInt16(l.wrapping_add(r)),
                (Val::UInt32(l), Val::UInt32(r)) => Val::UInt32(l.wrapping_add(r)),
                (Val::UInt64(l), Val::UInt64(r)) => Val::UInt64(l.wrapping_add(r)),

                (Val::Int8(l), Val::Int8(r)) => Val::Int8(l.wrapping_add(r)),
                (Val::Int16(l), Val::Int16(r)) => Val::Int16(l.wrapping_add(r)),
                (Val::Int32(l), Val::Int32(r)) => Val::Int32(l.wrapping_add(r)),
                (Val::Int64(l), Val::Int64(r)) => Val::Int64(l.wrapping_add(r)),

                (Val::Float32(l), Val::Float32(r)) => Val::Float32(l + r),
                (Val::Float64(l), Val::Float64(r)) => Val::Float64(l + r),

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
                (Val::UInt8(l), Val::UInt8(r)) => Val::Bool(l == r),
                (Val::UInt16(l), Val::UInt16(r)) => Val::Bool(l == r),
                (Val::UInt32(l), Val::UInt32(r)) => Val::Bool(l == r),
                (Val::UInt64(l), Val::UInt64(r)) => Val::Bool(l == r),

                (Val::Int8(l), Val::Int8(r)) => Val::Bool(l == r),
                (Val::Int16(l), Val::Int16(r)) => Val::Bool(l == r),
                (Val::Int32(l), Val::Int32(r)) => Val::Bool(l == r),
                (Val::Int64(l), Val::Int64(r)) => Val::Bool(l == r),

                (Val::Float32(l), Val::Float32(r)) => Val::Bool(l == r),
                (Val::Float64(l), Val::Float64(r)) => Val::Bool(l == r),

                _ => panic!("type error"),
            },
            BinOp::Neq => match (left, right) {
                (Val::UInt8(l), Val::UInt8(r)) => Val::Bool(l != r),
                (Val::UInt16(l), Val::UInt16(r)) => Val::Bool(l != r),
                (Val::UInt32(l), Val::UInt32(r)) => Val::Bool(l != r),
                (Val::UInt64(l), Val::UInt64(r)) => Val::Bool(l != r),

                (Val::Int8(l), Val::Int8(r)) => Val::Bool(l != r),
                (Val::Int16(l), Val::Int16(r)) => Val::Bool(l != r),
                (Val::Int32(l), Val::Int32(r)) => Val::Bool(l != r),
                (Val::Int64(l), Val::Int64(r)) => Val::Bool(l != r),

                (Val::Float32(l), Val::Float32(r)) => Val::Bool(l != r),
                (Val::Float64(l), Val::Float64(r)) => Val::Bool(l != r),

                _ => panic!("type error"),
            },
            BinOp::Lt => match (left, right) {
                (Val::UInt8(l), Val::UInt8(r)) => Val::Bool(l < r),
                (Val::UInt16(l), Val::UInt16(r)) => Val::Bool(l < r),
                (Val::UInt32(l), Val::UInt32(r)) => Val::Bool(l < r),
                (Val::UInt64(l), Val::UInt64(r)) => Val::Bool(l < r),

                (Val::Int8(l), Val::Int8(r)) => Val::Bool(l < r),
                (Val::Int16(l), Val::Int16(r)) => Val::Bool(l < r),
                (Val::Int32(l), Val::Int32(r)) => Val::Bool(l < r),
                (Val::Int64(l), Val::Int64(r)) => Val::Bool(l < r),

                (Val::Float32(l), Val::Float32(r)) => Val::Bool(l < r),
                (Val::Float64(l), Val::Float64(r)) => Val::Bool(l < r),

                _ => panic!("type error"),
            },
            BinOp::Leq => match (left, right) {
                (Val::UInt8(l), Val::UInt8(r)) => Val::Bool(l <= r),
                (Val::UInt16(l), Val::UInt16(r)) => Val::Bool(l <= r),
                (Val::UInt32(l), Val::UInt32(r)) => Val::Bool(l <= r),
                (Val::UInt64(l), Val::UInt64(r)) => Val::Bool(l <= r),

                (Val::Int8(l), Val::Int8(r)) => Val::Bool(l <= r),
                (Val::Int16(l), Val::Int16(r)) => Val::Bool(l <= r),
                (Val::Int32(l), Val::Int32(r)) => Val::Bool(l <= r),
                (Val::Int64(l), Val::Int64(r)) => Val::Bool(l <= r),

                (Val::Float32(l), Val::Float32(r)) => Val::Bool(l <= r),
                (Val::Float64(l), Val::Float64(r)) => Val::Bool(l <= r),

                _ => panic!("type error"),
            },
            BinOp::Gt => match (left, right) {
                (Val::UInt8(l), Val::UInt8(r)) => Val::Bool(l > r),
                (Val::UInt16(l), Val::UInt16(r)) => Val::Bool(l > r),
                (Val::UInt32(l), Val::UInt32(r)) => Val::Bool(l > r),
                (Val::UInt64(l), Val::UInt64(r)) => Val::Bool(l > r),

                (Val::Int8(l), Val::Int8(r)) => Val::Bool(l > r),
                (Val::Int16(l), Val::Int16(r)) => Val::Bool(l > r),
                (Val::Int32(l), Val::Int32(r)) => Val::Bool(l > r),
                (Val::Int64(l), Val::Int64(r)) => Val::Bool(l > r),

                (Val::Float32(l), Val::Float32(r)) => Val::Bool(l > r),
                (Val::Float64(l), Val::Float64(r)) => Val::Bool(l > r),

                _ => panic!("type error"),
            },
            BinOp::Geq => match (left, right) {
                (Val::UInt8(l), Val::UInt8(r)) => Val::Bool(l >= r),
                (Val::UInt16(l), Val::UInt16(r)) => Val::Bool(l >= r),
                (Val::UInt32(l), Val::UInt32(r)) => Val::Bool(l >= r),
                (Val::UInt64(l), Val::UInt64(r)) => Val::Bool(l >= r),

                (Val::Int8(l), Val::Int8(r)) => Val::Bool(l >= r),
                (Val::Int16(l), Val::Int16(r)) => Val::Bool(l >= r),
                (Val::Int32(l), Val::Int32(r)) => Val::Bool(l >= r),
                (Val::Int64(l), Val::Int64(r)) => Val::Bool(l >= r),

                (Val::Float32(l), Val::Float32(r)) => Val::Bool(l >= r),
                (Val::Float64(l), Val::Float64(r)) => Val::Bool(l >= r),

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

    fn eval_fn_call(&mut self, func: ir::Fn, args: Vec<Val>) -> Val {
        self.variables.enter_scope();
        for (param, arg) in iter::zip(func.params.iter().copied(), args) {
            self.variables.define(param, arg);
        }
        let rtn = match self.eval(&func.body) {
            EvalResult::Value(val) => val,
            EvalResult::Continue => panic!("can't continue outside of loop"),
            EvalResult::Break(val) => panic!("can't break outside of loop"),
            EvalResult::Return(val) => val,
        };
        self.variables.exit_scope();
        rtn
    }
}

fn val_from_literal(literal: &Literal) -> Val {
    match &literal.kind {
        LiteralKind::Bool(_) => todo!(),
        LiteralKind::Int(v) => Val::Int32(*v as _),
        LiteralKind::Float(symbol) => todo!(),
        LiteralKind::Fn(func) => Val::Func(func.clone()),
    }
}

/// Mana value
#[derive(Debug, Clone)]
pub(crate) enum Val {
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
    Func(ir::Fn),
}
