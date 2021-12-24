use crate::{
    ast::{self, Ident, Span, TypePath},
    intern::SymbolInterner,
    ty::{Ty, TyInterner},
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

pub struct LoweringContext {
    ty_interner: TyInterner,
    symbol_interner: SymbolInterner,
}

impl LoweringContext {
    fn resolve_typepath(&self, typepath: &TypePath) -> LoweringResult<Type<'_>> {
        // TODO: This is kinda silly, that we go from string to vec to string again. We could probably
        // just store TypePath as a string?
        let path = typepath
            .path
            .iter()
            .map(|ident| self.symbol_interner.resolve(&ident.name))
            .intersperse(".")
            .collect::<String>();
        let ty = self
            .ty_interner
            .resolve(&path)
            .ok_or_else(|| LoweringError {
                kind: LoweringErrorKind::UnknownType(path),
                span: typepath.span,
            })?;
        Ok(Type { ty, span: typepath.span })
    }

    fn lower_fn_def(&self, fd: &ast::FnDef) -> LoweringResult<FnDef> {
        let mut params = Vec::with_capacity(fd.params.len());
        for (ident, typepath) in &fd.params {
            params.push((*ident, self.resolve_typepath(&typepath)?));
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

    fn lower_expr(&self, expr: &ast::Expr) -> LoweringResult<Expr> {
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
            ast::ExprKind::Let(ident, typepath, expr) => {
                self.lower_let(ident, typepath.as_ref(), expr)
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
            ast::ExprKind::If { cond, then_expr, else_expr } => todo!(),
        }
    }

    fn lower_binary(
        &self,
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

    fn lower_unary(&self, op: &ast::UnaryOp, expr: &ast::Expr) -> LoweringResult<Expr> {
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
        ident: &ast::Ident,
        typepath: Option<&ast::TypePath>,
        expr: &ast::Expr,
    ) -> LoweringResult<Expr> {
        todo!()
    }

    fn lower_literal(&self, lit: &ast::Literal) -> LoweringResult<Literal> {
        // TODO: Maybe consider the literal a bit more
        let (kind, ty) = match lit.kind {
            ast::LiteralKind::Bool => (LiteralKind::Bool, self.ty_interner.bool()),
            ast::LiteralKind::Int => (LiteralKind::Int, self.ty_interner.resolve("Int").unwrap()),
            ast::LiteralKind::Float => (
                LiteralKind::Float,
                self.ty_interner.resolve("Float64").unwrap(),
            ),
            ast::LiteralKind::String => (
                LiteralKind::String,
                self.ty_interner.resolve("String").unwrap(),
            ),
        };
        Ok(Literal { kind, ty, span: lit.span })
    }

    fn lower_block(&self, block: &ast::Block) -> LoweringResult<(Ty, Vec<Expr>)> {
        let mut ty = self.ty_interner.unit();
        let mut exprs = Vec::with_capacity(block.len());
        for expr in block.iter() {
            let expr = self.lower_expr(expr)?;
            ty = expr.ty;
            exprs.push(expr);
        }
        Ok((ty, exprs))
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

    Let(Ident, Type<'ctx>, Box<Expr<'ctx>>),
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
    Bool,
    Int,
    Float,
    String,
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
