use crate::{
    ast::{self, IdentPath, Span},
    resolve::{Resolver, ResolverError},
    ty::{FloatTy, FnTy, IntTy, TyKind, Type, UIntTy},
};

use super::{
    registry::{Registry, TypeId},
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

pub struct LoweredModule {
    pub ir: Module,
    pub registry: Registry,
}

pub fn lower_module(
    module: &ast::Module,
    symbols: &mut SymbolInterner,
) -> Result<LoweredModule, LoweringError> {
    let mut registry = Registry::with_primitives();
    let resolver = resolver_with_prelude(symbols, &mut registry);

    let mut lowerer = Lowerer { resolver, registry, symbols };
    let module = lowerer.lower_module(module)?;
    Ok(LoweredModule {
        ir: module,
        registry: lowerer.registry,
    })
}

/// Initialize a Resolver with a Prelude
fn resolver_with_prelude(
    interner: &mut SymbolInterner,
    registry: &mut Registry,
) -> Resolver<ObjectId> {
    let mut sym = |s: &str| interner.get_or_intern(s);

    let mut resolver = Resolver::new();

    // primitives
    for (sym, ty_id) in [
        (sym("Bool"), registry.bool()),
        (sym("Int"), registry.int()),
        (sym("UInt"), registry.uint()),
        (sym("Float"), registry.float()),
    ] {
        resolver.define(sym, ObjectId::Type(ty_id));
    }

    // prelude
    for (sym, ty_kind) in [
        // signed int
        (sym("Int8"), TyKind::Int(IntTy::I8)),
        (sym("Int16"), TyKind::Int(IntTy::I16)),
        (sym("Int32"), TyKind::Int(IntTy::I32)),
        (sym("Int64"), TyKind::Int(IntTy::I64)),
        (sym("ISize"), TyKind::Int(IntTy::ISize)),
        // unsigned int
        (sym("UInt8"), TyKind::UInt(UIntTy::U8)),
        (sym("UInt16"), TyKind::UInt(UIntTy::U16)),
        (sym("UInt32"), TyKind::UInt(UIntTy::U32)),
        (sym("UInt64"), TyKind::UInt(UIntTy::U64)),
        (sym("USize"), TyKind::UInt(UIntTy::USize)),
        // floats
        (sym("Float32"), TyKind::Float(FloatTy::F32)),
        (sym("Float64"), TyKind::Float(FloatTy::F64)),
    ] {
        let type_id = registry.register_type(Type { kind: ty_kind });
        resolver.define(sym, ObjectId::Type(type_id));
    }

    resolver
}

struct Lowerer<'ctx> {
    resolver: Resolver<ObjectId>,
    registry: Registry,
    symbols: &'ctx SymbolInterner,
}

// TODO: Unify FunctionId and VariableIds
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ObjectId {
    Type(TypeId),
    Variable(VariableId),
}

impl ObjectId {
    pub fn as_type_id(self) -> Option<TypeId> {
        if let Self::Type(id) = self {
            Some(id)
        } else {
            None
        }
    }

    pub fn as_variable_id(self) -> Option<VariableId> {
        if let Self::Variable(id) = self {
            Some(id)
        } else {
            None
        }
    }
}

impl<'ctx> Lowerer<'ctx> {
    fn resolve_typepath(&self, typepath: &IdentPath) -> LoweringResult<TypePath> {
        // TODO: This is a hack
        let sym = typepath.path[0].sym;
        let obj = self.resolver.resolve(sym).ok_or(LoweringError {
            kind: LoweringErrorKind::UnknownType("TODO".to_owned()),
            span: typepath.span,
        })?;

        let ty = obj.as_type_id().ok_or(LoweringError {
            kind: LoweringErrorKind::UnknownType("TODO".to_owned()),
            span: typepath.span,
        })?;

        Ok(TypePath { id: ty, span: typepath.span })
    }
}

impl<'ctx> Lowerer<'ctx> {
    fn lower_module(&mut self, module: &ast::Module) -> LoweringResult<Module> {
        self.resolver.enter_scope();
        let mut module_items = Vec::with_capacity(module.items.len());

        // TODO: Switch to lazy tree-shaking for consts. Make imports not an item

        // Declare all items
        let mut def_ids = Vec::new();
        for item in &module.items {
            match item {
                ast::Item::Error => unreachable!(),
                ast::Item::Def(fn_def) => {
                    let id = self.registry.declare_def(fn_def.name);
                    def_ids.push(id);
                    module_items.push(Item::Def(id));
                }
            }
        }

        // Define all items
        let mut def_ids = def_ids.into_iter();
        for item in &module.items {
            match item {
                ast::Item::Error => unreachable!(),
                ast::Item::Def(def) => {
                    let def_id = def_ids.next().unwrap();
                    let expr = self.lower_expr(&def.value)?;
                    self.registry.define_def(def_id, expr);
                }
            }
        }

        self.resolver.exit_scope();

        Ok(Module { items: module_items })
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> LoweringResult<Expr> {
        // TODO: I need to find a better way to track span better. Maybe lower_* returns ExprKind?
        match &expr.kind {
            ast::ExprKind::Error => unreachable!("cannot lower error ast"),

            ast::ExprKind::Ident(ident) => {
                let obj_id = self
                    .resolver
                    .resolve(ident.sym)
                    .expect("undefined variable. TODO: Better error message");
                let var_id = obj_id.as_variable_id().expect("this isn't a variable");
                Ok(Expr {
                    span: ident.span,
                    kind: ExprKind::Variable(var_id),
                    ty: self.registry.get_variable(var_id).type_id,
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
            ast::ExprKind::Index(_, _) => todo!("lower index"),
            ast::ExprKind::Access(_, _) => todo!("lower access"),

            ast::ExprKind::Let(var_id, typepath, init_expr) => {
                self.lower_let(expr.span, var_id, typepath.as_ref(), init_expr)
            }
            ast::ExprKind::Set(var_id, set_expr) => self.lower_set(expr.span, var_id, set_expr),

            ast::ExprKind::Loop(loop_expr) => self.lower_loop(expr.span, loop_expr),
            ast::ExprKind::Break(break_expr) => self.lower_break(expr.span, break_expr),
            ast::ExprKind::Continue(continue_expr) => self.lower_continue(expr.span, continue_expr),
            ast::ExprKind::Return(return_expr) => self.lower_return(expr.span, return_expr),

            ast::ExprKind::FnCall(ident, args) => self.lower_fn_call(expr.span, ident, args),
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
            ast::BinOp::Add => BinOp::Add,
            ast::BinOp::Sub => BinOp::Sub,
            ast::BinOp::Mul => BinOp::Mul,
            ast::BinOp::Div => BinOp::Div,
            ast::BinOp::Rem => BinOp::Rem,
            ast::BinOp::Band => BinOp::Band,
            ast::BinOp::Bor => BinOp::Bor,
            ast::BinOp::Bxor => BinOp::Bxor,

            ast::BinOp::Land => BinOp::Land,
            ast::BinOp::Lor => BinOp::Lor,

            ast::BinOp::Eq => BinOp::Eq,
            ast::BinOp::Neq => BinOp::Neq,

            ast::BinOp::Lt => BinOp::Lt,
            ast::BinOp::Leq => BinOp::Leq,
            ast::BinOp::Gt => BinOp::Gt,
            ast::BinOp::Geq => BinOp::Geq,
        };

        let left = self.lower_expr(left)?;
        let right = self.lower_expr(right)?;

        // Type check
        let ty = match op {
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Rem
            | BinOp::Band
            | BinOp::Bor
            | BinOp::Bxor => {
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
            ast::UnaryOp::Lnot => UnaryOp::Lnot,
            ast::UnaryOp::Bnot => UnaryOp::Bnot,
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

            UnaryOp::Lnot => {
                if !self.registry.get_type(expr.ty).is_boolean() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(expr.ty),
                        span: expr.span,
                    });
                }
                expr.ty
            }

            UnaryOp::Bnot => {
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
            .register_variable(Variable { type_id: expr.ty, ident: *ident });
        self.resolver
            .define(ident.sym, ObjectId::Variable(var_id))
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

        let var_id = self.resolver.resolve(ident.sym).ok_or(LoweringError {
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

    fn lower_loop(&mut self, span: Span, expr: &ast::Expr) -> LoweringResult<Expr> {
        Ok(Expr {
            span,
            kind: ExprKind::Loop(Box::new(self.lower_expr(expr)?)),
            // TODO: This type should be that of the break statement
            ty: self.registry.unit(),
        })
    }

    fn lower_break(&mut self, span: Span, expr: &Option<Box<ast::Expr>>) -> LoweringResult<Expr> {
        let expr = if let Some(expr) = expr {
            Some(Box::new(self.lower_expr(expr)?))
        } else {
            None
        };
        Ok(Expr {
            span,
            ty: self.registry.unit(),
            kind: ExprKind::Break(expr),
        })
    }

    fn lower_continue(
        &mut self,
        span: Span,
        expr: &Option<Box<ast::Expr>>,
    ) -> LoweringResult<Expr> {
        let expr = if let Some(expr) = expr {
            Some(Box::new(self.lower_expr(expr)?))
        } else {
            None
        };
        Ok(Expr {
            span,
            // TODO: This type should be that of the break statement
            ty: self.registry.unit(),
            kind: ExprKind::Continue(expr),
        })
    }

    fn lower_return(&mut self, span: Span, expr: &Option<Box<ast::Expr>>) -> LoweringResult<Expr> {
        let expr = if let Some(expr) = expr {
            Some(Box::new(self.lower_expr(expr)?))
        } else {
            None
        };
        Ok(Expr {
            span,
            // TODO: This should be noreturn or something maybe?
            ty: self.registry.unit(),
            kind: ExprKind::Return(expr),
        })
    }

    fn lower_fn_call(
        &mut self,
        span: Span,
        ident: &ast::Ident,
        args: &[ast::Expr],
    ) -> LoweringResult<Expr> {
        let id = self.resolver.resolve(ident.sym).ok_or(LoweringError {
            kind: LoweringErrorKind::UnknownVariable(*ident),
            span: ident.span,
        })?;
        let id = id
            .as_variable_id()
            .expect("must be function. TODO: rework entire diagnostics");
        let mut lowered_args = Vec::with_capacity(args.len());
        for expr in args {
            lowered_args.push(self.lower_expr(expr)?);
        }

        let var = self.registry.get_variable(id);
        let TyKind::Fn(fn_) = &self.registry.get_type(var.type_id).kind else {
            return Err(LoweringError {
                kind: LoweringErrorKind::InvalidType(var.type_id),
                span,
            });
        };

        Ok(Expr {
            span,
            ty: fn_.rtn,
            kind: ExprKind::FnCall(id, lowered_args),
        })
    }

    fn lower_literal(&mut self, lit: &ast::LiteralKind) -> LoweringResult<Literal> {
        let (kind, ty) = match lit {
            ast::LiteralKind::Bool(val) => (LiteralKind::Bool(*val), self.registry.bool()),
            ast::LiteralKind::Int(val) => (LiteralKind::Int(*val), self.registry.int()),
            ast::LiteralKind::Float(val) => (LiteralKind::Float(*val), self.registry.float()),
            ast::LiteralKind::String(_) => todo!(),
            ast::LiteralKind::Fn(val) => {
                let fn_ = self.lower_fn(val)?;
                let params_ty_ids = fn_
                    .params
                    .iter()
                    .copied()
                    .map(|vid| self.registry.get_variable(vid).type_id)
                    .collect();
                let ty = Type {
                    kind: TyKind::Fn(FnTy {
                        params: params_ty_ids,
                        rtn: fn_.return_typepath.id,
                    }),
                };
                let fn_ty_id = self.registry.register_type(ty);
                (LiteralKind::Fn(fn_), fn_ty_id)
            }
        };
        Ok(Literal { kind, ty })
    }

    fn lower_fn(&mut self, fn_: &ast::Fn) -> LoweringResult<Fn> {
        self.resolver.enter_scope();

        let mut params = Vec::with_capacity(fn_.params.len());
        for (ident, typepath) in &fn_.params {
            let ty = self.resolve_typepath(typepath)?;
            let var_id = self
                .registry
                .register_variable(Variable { ident: *ident, type_id: ty.id });
            params.push(var_id);
        }

        let (body_ty, body) = self.lower_block(&fn_.body)?;
        let return_typepath = if let Some(typepath) = &fn_.return_typepath {
            let typepath = self.resolve_typepath(typepath)?;
            if typepath.id != body_ty {
                return Err(LoweringError {
                    kind: LoweringErrorKind::TypeConflict { want: typepath.id, got: body_ty },
                    span: body.last().map(|e| e.span).unwrap(),
                });
            }
            typepath
        } else {
            TypePath {
                id: self.registry.unit(),
                // TODO: We can't get this right now
                span: (0, 0),
            }
        };

        self.resolver.exit_scope();

        Ok(Fn { params, return_typepath, body })
    }

    fn lower_block(&mut self, block: &[ast::Expr]) -> LoweringResult<(TypeId, Vec<Expr>)> {
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
