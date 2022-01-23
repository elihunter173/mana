use crate::{
    ast::{self, Ident, Span, TypePath},
    intern::{Symbol, SymbolInterner},
    resolve::{ManaObject, ManaPath, Resolver},
    ty::Ty,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LoweringError<'ctx> {
    pub kind: LoweringErrorKind<'ctx>,
    pub span: (usize, usize),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum LoweringErrorKind<'ctx> {
    UnknownType(String),
    TypeConflict { want: Ty<'ctx>, got: Ty<'ctx> },
    InvalidType(Ty<'ctx>),
}

type LoweringResult<'ctx, T> = Result<T, LoweringError<'ctx>>;

pub struct LoweringContext<'ctx> {
    pub ty_interner: &'ctx Resolver,
    pub symbol_interner: &'ctx SymbolInterner,
}

impl<'ctx> LoweringContext<'ctx> {
    fn resolve_typepath(&self, typepath: &TypePath) -> LoweringResult<Type<'ctx>> {
        // TODO: This is kinda silly, that we go from string to vec to string again. We could probably
        // just store TypePath as a string?
        let path = ManaPath {
            idents: typepath.path.iter().map(|ident| ident.name).collect(),
        };
        let obj = self.ty_interner.resolve(&path).ok_or(LoweringError {
            kind: LoweringErrorKind::UnknownType("TODO".to_owned()),
            span: typepath.span,
        })?;
        let ty = match obj {
            ManaObject::Type(ty) => ty,
            _ => {
                return Err(LoweringError {
                    kind: LoweringErrorKind::UnknownType("TODO".to_owned()),
                    span: typepath.span,
                })
            }
        };
        Ok(Type { ty, span: typepath.span })
    }

    // TODO: Think about what should be public

    pub fn lower_fn_def(&self, fd: &ast::FnDef) -> LoweringResult<FnDef<'ctx>> {
        let mut params = Vec::with_capacity(fd.params.len());
        for (ident, typepath) in &fd.params {
            params.push((*ident, self.resolve_typepath(typepath)?));
        }

        let return_ty = if let Some(typepath) = &fd.return_typepath {
            self.resolve_typepath(typepath)?
        } else {
            Type {
                ty: self.ty_interner.unit(),
                span: fd.name.span,
            }
        };

        let mut body = Vec::with_capacity(fd.body.len());
        for expr in &fd.body {
            body.push(self.lower_expr(expr)?);
        }

        Ok(FnDef {
            name: fd.name,
            params,
            return_ty,
            body,
        })
    }

    fn lower_expr(&self, expr: &ast::Expr) -> LoweringResult<Expr<'ctx>> {
        match &expr.kind {
            ast::ExprKind::Ident(_) => todo!("I need to respect lexical scope for this"),
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
            // TODO: This should respect scope
            ast::ExprKind::Let(ident, typepath, init_expr) => {
                self.lower_let(expr.span, ident, typepath.as_ref(), init_expr)
            }
            ast::ExprKind::Set(_, _) => todo!("I need to add the concept of scope for this"),
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
                else_expr.as_ref().expect("TODO: support empty else"),
            ),
        }
    }

    fn lower_binary(
        &self,
        op: &ast::BinOp,
        left: &ast::Expr,
        right: &ast::Expr,
    ) -> LoweringResult<Expr<'ctx>> {
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
                if !left.ty.is_numeric() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(left.ty),
                        span: left.span,
                    });
                }
                if !right.ty.is_numeric() {
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

                self.ty_interner.bool()
            }

            BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => {
                if !left.ty.is_numeric() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(left.ty),
                        span: left.span,
                    });
                }
                if !right.ty.is_numeric() {
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

                self.ty_interner.bool()
            }

            BinOp::Land | BinOp::Lor => {
                if !left.ty.is_boolean() {
                    return Err(LoweringError {
                        kind: LoweringErrorKind::InvalidType(left.ty),
                        span: left.span,
                    });
                }
                if !right.ty.is_boolean() {
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

    fn lower_unary(&self, op: &ast::UnaryOp, expr: &ast::Expr) -> LoweringResult<Expr<'ctx>> {
        let op = match op {
            ast::UnaryOp::Neg => UnaryOp::Neg,
        };

        let expr = self.lower_expr(expr)?;

        let ty = match op {
            UnaryOp::Neg => {
                if !expr.ty.is_numeric() {
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
        &self,
        span: Span,
        ident: &ast::Ident,
        typepath: Option<&ast::TypePath>,
        expr: &ast::Expr,
    ) -> LoweringResult<Expr<'ctx>> {
        let expr = self.lower_expr(expr)?;
        if let Some(typepath) = typepath {
            let declared_typ = self.resolve_typepath(typepath)?;
            if declared_typ.ty != expr.ty {
                return Err(LoweringError {
                    kind: LoweringErrorKind::TypeConflict { want: declared_typ.ty, got: expr.ty },
                    span: expr.span,
                });
            }
        }

        Ok(Expr {
            span,
            ty: self.ty_interner.unit(),
            kind: ExprKind::Let(*ident, Box::new(expr)),
        })
    }

    fn lower_literal(&self, lit: &ast::Literal) -> LoweringResult<Literal<'ctx>> {
        let (kind, ty) = match lit.kind {
            ast::LiteralKind::Bool(val) => (LiteralKind::Bool(val), self.ty_interner.bool()),
            ast::LiteralKind::Int(val) => (LiteralKind::Int(val), self.ty_interner.int()),
            ast::LiteralKind::Float(val) => (LiteralKind::Float(val), self.ty_interner.float()),
            ast::LiteralKind::String(val) => (LiteralKind::String(val), self.ty_interner.string()),
        };
        Ok(Literal { kind, ty, span: lit.span })
    }

    fn lower_block(&self, block: &ast::Block) -> LoweringResult<(Ty<'ctx>, Vec<Expr<'ctx>>)> {
        let mut ty = self.ty_interner.unit();
        let mut exprs = Vec::with_capacity(block.len());
        for expr in block.iter() {
            let expr = self.lower_expr(expr)?;
            ty = expr.ty;
            exprs.push(expr);
        }
        Ok((ty, exprs))
    }

    fn lower_if(
        &self,
        span: Span,
        cond: &ast::Expr,
        then_expr: &ast::Expr,
        else_expr: &ast::Expr,
    ) -> LoweringResult<Expr<'ctx>> {
        let cond = self.lower_expr(cond)?;
        if !cond.ty.is_boolean() {
            return Err(LoweringError {
                kind: LoweringErrorKind::TypeConflict {
                    want: self.ty_interner.bool(),
                    got: cond.ty,
                },
                span: cond.span,
            });
        }

        let then_expr = self.lower_expr(then_expr)?;
        let else_expr = self.lower_expr(else_expr)?;
        if then_expr.ty != else_expr.ty {
            return Err(LoweringError {
                kind: LoweringErrorKind::TypeConflict {
                    want: then_expr.ty,
                    got: else_expr.ty,
                },
                span: else_expr.span,
            });
        }

        Ok(Expr {
            span,
            ty: then_expr.ty,
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Some(Box::new(else_expr)),
            },
        })
    }
}

/// A resolved TypePath. `span` is where the typepath is
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type<'ctx> {
    pub ty: Ty<'ctx>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnDef<'ctx> {
    pub name: Ident,
    pub params: Vec<(Ident, Type<'ctx>)>,
    pub return_ty: Type<'ctx>,
    pub body: Vec<Expr<'ctx>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr<'ctx> {
    pub span: Span,
    pub kind: ExprKind<'ctx>,
    pub ty: Ty<'ctx>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind<'ctx> {
    Ident(Ident),
    Literal(Literal<'ctx>),
    Binary(BinOp, Box<Expr<'ctx>>, Box<Expr<'ctx>>),
    Unary(UnaryOp, Box<Expr<'ctx>>),

    Let(Ident, Box<Expr<'ctx>>),
    Set(Ident, Box<Expr<'ctx>>),

    FnCall(Ident, Vec<Expr<'ctx>>),
    Block(Block<'ctx>),
    If {
        cond: Box<Expr<'ctx>>,
        // TODO: Maybe make this a Block type?
        then_expr: Box<Expr<'ctx>>,
        else_expr: Option<Box<Expr<'ctx>>>,
    },
}

pub type Block<'ctx> = Vec<Expr<'ctx>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal<'ctx> {
    pub span: Span,
    pub kind: LiteralKind,
    pub ty: Ty<'ctx>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Bool(bool),
    Int(u128),
    Float(Symbol),
    String(Symbol),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Mul,
    Div,
    Add,
    Sub,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Land,
    Lor,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}
