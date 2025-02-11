use crate::{
    intern::SymbolInterner,
    ir::{
        self,
        registry::{Registry, VariableId},
    },
};

pub struct Machine<'ctx> {
    module: ir::Module,

    symbols: &'ctx SymbolInterner,
    registry: &'ctx Registry,
}

impl Machine<'_> {
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

        self.eval(main)
    }

    pub fn eval(&mut self, expr: &ir::Expr) -> ManaValue {
        match &expr.kind {
            ir::ExprKind::Variable(variable_id) => todo!(),
            ir::ExprKind::Literal(literal) => todo!(),
            ir::ExprKind::Binary(bin_op, left, right) => todo!(),
            ir::ExprKind::Unary(unary_op, expr) => todo!(),
            ir::ExprKind::Let(variable_id, expr) => todo!(),
            ir::ExprKind::Set(variable_id, expr) => todo!(),
            ir::ExprKind::Loop(expr) => todo!(),
            ir::ExprKind::Break(expr) => todo!(),
            ir::ExprKind::Continue(expr) => todo!(),
            ir::ExprKind::Return(expr) => todo!(),
            ir::ExprKind::FnCall(variable_id, exprs) => todo!(),
            ir::ExprKind::Block(exprs) => todo!(),
            ir::ExprKind::If { cond, then_expr, else_expr } => match self.eval(cond) {
                ManaValue::Bool(true) => self.eval(&then_expr),
                ManaValue::Bool(false) => {
                    if let Some(else_expr) = else_expr {
                        self.eval(&else_expr)
                    } else {
                        ManaValue::Unit
                    }
                }
                _ => panic!("type error"),
            },
        }
    }
}

enum ManaValue {
    Unit,
    Bool(bool),
}
