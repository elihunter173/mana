use crate::{
    ast::{self, IdentPath, Span},
    ir::resolve::ResolverError,
};

use super::{
    registry::{Registry, TypeId},
    resolve::Resolver,
    *,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LoweringError {
    pub kind: LoweringErrorKind,
    pub span: (usize, usize),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum LoweringErrorKind {
    UnknownType(String),
    UnknownVariable(Ident),
    TypeConflict { want: TypeId, got: TypeId },
    InvalidType(TypeId),
    DuplicateItem,
}

type LoweringResult<T> = Result<T, LoweringError>;

pub struct Lowerer<'ctx> {
    // TODO: These should not be public
    pub resolver: &'ctx mut Resolver,
    pub registry: &'ctx mut Registry,
    pub symbol_interner: &'ctx SymbolInterner,
}

impl<'ctx> Lowerer<'ctx> {
    // TODO: TypePaths shouldn't exist?
    fn resolve_typepath(&self, typepath: &IdentPath) -> LoweringResult<Type> {
        // TODO: This is a hack
        let sym = typepath.path[0].sym;
        let obj = self.resolver.resolve(&sym).ok_or(LoweringError {
            kind: LoweringErrorKind::UnknownType("TODO".to_owned()),
            span: typepath.span,
        })?;

        let ty = obj.as_type_id().ok_or(LoweringError {
            kind: LoweringErrorKind::UnknownType("TODO".to_owned()),
            span: typepath.span,
        })?;

        Ok(Type { id: ty, span: typepath.span })
    }
}

impl<'ctx> Lowerer<'ctx> {
    pub fn lower_module(&mut self, module: &ast::Module) -> LoweringResult<Module> {
        self.resolver.enter_scope();
        let mut module_items = Vec::with_capacity(module.items.len());
        for item in &module.items {
            match item {
                ast::Item::FnDef(fn_def) => {
                    let func = self.lower_fn_def(fn_def)?;
                    let func_span = func.span;
                    let func_name = func.name.sym;
                    let func_id = self.registry.register_function(func);
                    self.resolver
                        .define_function(func_name, func_id)
                        .map_err(|err| match err {
                            ResolverError::DuplicateItem => LoweringError {
                                kind: LoweringErrorKind::DuplicateItem,
                                span: func_span,
                            },
                        })?;
                    module_items.push(Item::Function(func_id))
                }
                ast::Item::Import(_) => unimplemented!(),
            }
        }
        self.resolver.exit_scope();
        Ok(Module { items: module_items })
    }

    fn lower_fn_def(&mut self, fn_def: &ast::FnDef) -> LoweringResult<Function> {
        let mut params = Vec::with_capacity(fn_def.params.len());
        for (ident, typepath) in &fn_def.params {
            params.push((*ident, self.resolve_typepath(typepath)?));
        }

        let return_ty = if let Some(typepath) = &fn_def.return_typepath {
            self.resolve_typepath(typepath)?
        } else {
            Type {
                id: self.registry.unit(),
                span: fn_def.name.span,
            }
        };

        let mut body = Vec::with_capacity(fn_def.body.len());
        for expr in &fn_def.body {
            body.push(self.lower_expr(expr)?);
        }

        Ok(Function {
            name: fn_def.name,
            params,
            return_ty,
            body,
            span: fn_def.span,
        })
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> LoweringResult<Expr> {
        match &expr.kind {
            ast::ExprKind::Ident(ident) => {
                let obj_id = self
                    .resolver
                    .resolve(&ident.sym)
                    .expect("undefined variable. TODO: Better error message");
                let var_id = obj_id.as_variable_id().expect("this isn't a variable");
                Ok(Expr {
                    span: ident.span,
                    kind: ExprKind::Variable(var_id),
                    ty: self.registry.get_variable(var_id).ty,
                })
            }

            ast::ExprKind::Literal(lit) => {
                let lowered = self.lower_literal(lit)?;
                Ok(Expr {
                    span: expr.span,
                    ty: lowered.ty,
                    kind: ExprKind::Literal(lowered),
                })
            }
            ast::ExprKind::Binary(op, left, right) => self.lower_binary(op, left, right),
            ast::ExprKind::Unary(op, expr) => self.lower_unary(op, expr),
            ast::ExprKind::Let(var_id, typepath, init_expr) => {
                self.lower_let(expr.span, var_id, typepath.as_ref(), init_expr)
            }
            ast::ExprKind::Set(var_id, set_expr) => self.lower_set(expr.span, var_id, set_expr),
            ast::ExprKind::FnCall(_, _) => todo!("I need to add the concept of scope for this"),
            ast::ExprKind::Block(block) => {
                let (ty, block) = self.lower_block(block)?;
                Ok(Expr {
                    span: expr.span,
                    kind: ExprKind::Block(block),
                    ty,
                })
            }
            ast::ExprKind::If { cond, then_expr, else_expr } => self.lower_if(
                expr.span,
                cond,
                then_expr,
                else_expr.as_ref().map(|expr| expr.as_ref()),
            ),
        }
    }

    fn lower_binary(
        &mut self,
        op: &ast::BinOp,
        left: &ast::Expr,
        right: &ast::Expr,
    ) -> LoweringResult<Expr> {
        let op = match op {
            ast::BinOp::Mul => BinOp::Mul,
            ast::BinOp::Div => BinOp::Div,
            ast::BinOp::Add => BinOp::Add,
            ast::BinOp::Sub => BinOp::Sub,
            ast::BinOp::Eq => BinOp::Eq,
            ast::BinOp::Neq => BinOp::Neq,
            ast::BinOp::Lt => BinOp::Lt,
            ast::BinOp::Leq => BinOp::Leq,
            ast::BinOp::Gt => BinOp::Gt,
            ast::BinOp::Geq => BinOp::Geq,
            ast::BinOp::Land => BinOp::Land,
            ast::BinOp::Lor => BinOp::Lor,
        };

        let left = self.lower_expr(left)?;
        let right = self.lower_expr(right)?;

        // Type check
        let ty = match op {
            BinOp::Mul | BinOp::Div | BinOp::Add | BinOp::Sub => {
                // TODO: Eventually this should use our function call machinery
                if !self.registry.get_type(left.ty).is_numeric() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(left.ty),
                        span: left.span,
                    });
                }
                if !self.registry.get_type(right.ty).is_numeric() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(right.ty),
                        span: right.span,
                    });
                }
                if left.ty != right.ty {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::TypeConflict { want: left.ty, got: right.ty },
                        span: right.span,
                    });
                }

                left.ty
            }

            BinOp::Eq | BinOp::Neq => {
                if left.ty != right.ty {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::TypeConflict { want: left.ty, got: right.ty },
                        span: right.span,
                    });
                }

                self.registry.bool()
            }

            BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => {
                if !self.registry.get_type(left.ty).is_numeric() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(left.ty),
                        span: left.span,
                    });
                }
                if !self.registry.get_type(right.ty).is_numeric() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(right.ty),
                        span: right.span,
                    });
                }
                if left.ty != right.ty {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::TypeConflict { want: left.ty, got: right.ty },
                        span: right.span,
                    });
                }

                self.registry.bool()
            }

            BinOp::Land | BinOp::Lor => {
                if !self.registry.get_type(left.ty).is_boolean() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(left.ty),
                        span: left.span,
                    });
                }
                if !self.registry.get_type(right.ty).is_boolean() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(right.ty),
                        span: right.span,
                    });
                }

                left.ty
            }
        };

        Ok(Expr {
            span: (left.span.0, right.span.1),
            kind: ExprKind::Binary(op, Box::new(left), Box::new(right)),
            ty,
        })
    }

    fn lower_unary(&mut self, op: &ast::UnaryOp, expr: &ast::Expr) -> LoweringResult<Expr> {
        let op = match op {
            ast::UnaryOp::Neg => UnaryOp::Neg,
        };

        let expr = self.lower_expr(expr)?;

        let ty = match op {
            UnaryOp::Neg => {
                if !self.registry.get_type(expr.ty).is_numeric() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(expr.ty),
                        span: expr.span,
                    });
                }

                expr.ty
            }
        };

        Ok(Expr {
            span: expr.span,
            kind: ExprKind::Unary(op, Box::new(expr)),
            ty,
        })
    }

    fn lower_let(
        &mut self,
        span: Span,
        ident: &ast::Ident,
        typepath: Option<&ast::IdentPath>,
        expr: &ast::Expr,
    ) -> LoweringResult<Expr> {
        let expr = self.lower_expr(expr)?;
        if let Some(typepath) = typepath {
            let declared_type = self.resolve_typepath(typepath)?;
            if declared_type.id != expr.ty {
                return Err(LoweringError {
                    kind: LoweringErrorKind::TypeConflict { want: declared_type.id, got: expr.ty },
                    span: expr.span,
                });
            }
        }

        let var_id = self
            .registry
            .register_variable(Variable { ty: expr.ty, span: ident.span });
        self.resolver
            .define_variable(ident.sym, var_id)
            .map_err(|err| match err {
                ResolverError::DuplicateItem => LoweringError {
                    kind: LoweringErrorKind::DuplicateItem,
                    span: ident.span,
                },
            })?;

        Ok(Expr {
            span,
            ty: self.registry.unit(),
            kind: ExprKind::Let(var_id, Box::new(expr)),
        })
    }

    fn lower_set(
        &mut self,
        span: Span,
        ident: &ast::Ident,
        expr: &ast::Expr,
    ) -> LoweringResult<Expr> {
        let expr = self.lower_expr(expr)?;

        let var_id = self.resolver.resolve(&ident.sym).ok_or(LoweringError {
            kind: LoweringErrorKind::UnknownVariable(*ident),
            span: ident.span,
        })?;
        let var_id = var_id
            .as_variable_id()
            .expect("must be variable. TODO: rework entire diagnostics");

        Ok(Expr {
            span,
            ty: self.registry.unit(),
            kind: ExprKind::Set(var_id, Box::new(expr)),
        })
    }

    fn lower_literal(&self, lit: &ast::Literal) -> LoweringResult<Literal> {
        let (kind, ty) = match lit.kind {
            ast::LiteralKind::Bool(val) => (LiteralKind::Bool(val), self.registry.bool()),
            ast::LiteralKind::Int(val) => (LiteralKind::Int(val), self.registry.int()),
            ast::LiteralKind::Float(val) => (LiteralKind::Float(val), self.registry.float()),
            ast::LiteralKind::String(val) => (LiteralKind::String(val), self.registry.string()),
        };
        Ok(Literal { kind, ty, span: lit.span })
    }

    fn lower_block(&mut self, block: &ast::Block) -> LoweringResult<(TypeId, Vec<Expr>)> {
        self.resolver.enter_scope();
        let mut ty = self.registry.unit();
        let mut exprs = Vec::with_capacity(block.len());
        for expr in block.iter() {
            let expr = self.lower_expr(expr)?;
            ty = expr.ty;
            exprs.push(expr);
        }
        self.resolver.exit_scope();
        Ok((ty, exprs))
    }

    fn lower_if(
        &mut self,
        span: Span,
        cond: &ast::Expr,
        then_expr: &ast::Expr,
        else_expr: Option<&ast::Expr>,
    ) -> LoweringResult<Expr> {
        let cond = self.lower_expr(cond)?;
        if !self.registry.get_type(cond.ty).is_boolean() {
            return Err(LoweringError {
                kind: LoweringErrorKind::TypeConflict {
                    want: self.registry.bool(),
                    got: cond.ty,
                },
                span: cond.span,
            });
        }

        let then_expr = self.lower_expr(then_expr)?;
        let else_expr = if let Some(else_expr) = else_expr {
            Some(self.lower_expr(else_expr)?)
        } else {
            None
        };

        if let Some(else_expr) = &else_expr {
            if then_expr.ty != else_expr.ty {
                return Err(LoweringError {
                    kind: LoweringErrorKind::TypeConflict {
                        want: then_expr.ty,
                        got: else_expr.ty,
                    },
                    span: else_expr.span,
                });
            }
        } else {
            if then_expr.ty != self.registry.unit() {
                return Err(LoweringError {
                    kind: LoweringErrorKind::TypeConflict {
                        want: self.registry.unit(),
                        got: then_expr.ty,
                    },
                    span: then_expr.span,
                });
            }
        }

        Ok(Expr {
            span,
            ty: then_expr.ty,
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: else_expr.map(Box::new),
            },
        })
    }
}
