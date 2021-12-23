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
    UnknownType(TypePath),
    TypeConflict { want: Type<'ctx>, got: Type<'ctx> },
}

type LoweringResult<'ctx, T> = Result<T, LoweringError<'ctx>>;

struct LoweringContext {
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
                kind: LoweringErrorKind::UnknownType(typepath.clone()),
                span: typepath.span,
            })?;
        Ok(Type { ty, span: typepath.span })
    }

    fn lower_fn_def(&self, fd: ast::FnDef) -> LoweringResult<FnDef> {
        let mut params = Vec::with_capacity(fd.params.len());
        for (ident, typepath) in fd.params.into_iter() {
            params.push((ident, self.resolve_typepath(&typepath)?));
        }

        let return_ty = if let Some(typepath) = fd.return_typepath {
            self.resolve_typepath(&typepath)?
        } else {
            Type {
                ty: self.ty_interner.unit_ty_hack(),
                span: fd.name.span,
            }
        };

        let mut body = Vec::with_capacity(fd.body.len());
        for expr in fd.body.into_iter() {
            body.push(self.lower_expr(expr)?);
        }

        Ok(FnDef {
            name: fd.name,
            params,
            return_ty,
            body,
        })
    }

    fn lower_expr(&self, expr: ast::Expr) -> LoweringResult<Expr> {
        match expr.kind {
            ast::ExprKind::Ident(_) => todo!("I need to respect lexical scope here"),
            ast::ExprKind::Literal(lit) => {
                let span = lit.span;
                let lowered = self.lower_literal(lit)?;
                Ok(Expr {
                    span,
                    ty: lowered.ty,
                    kind: ExprKind::Literal(lowered),
                })
            }
            ast::ExprKind::Binary(op, left, right) => todo!(),
            ast::ExprKind::Unary(_, _) => todo!(),
            ast::ExprKind::Let(_, _, _) => todo!(),
            ast::ExprKind::Set(_, _) => todo!(),
            ast::ExprKind::FnCall(_, _) => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::If { cond, then_expr, else_expr } => todo!(),
        }
    }

    fn lower_binary(
        &self,
        op: ast::BinOp,
        left: ast::Expr,
        right: ast::Expr,
    ) -> (BinOp, Box<Expr<'_>>, Box<Expr<'_>>) {
        todo!()
    }

    fn lower_literal(&self, lit: ast::Literal) -> LoweringResult<Literal> {
        todo!()
    }

    fn lower_block(&self, block: ast::Block) -> LoweringResult<Literal> {
        todo!()
    }
}

/// A resolved TypePath. `span` is where the typepath is
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type<'ctx> {
    ty: Ty<'ctx>,
    span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnDef<'ctx> {
    name: Ident,
    params: Vec<(Ident, Type<'ctx>)>,
    return_ty: Type<'ctx>,
    body: Vec<Expr<'ctx>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr<'ctx> {
    span: Span,
    kind: ExprKind<'ctx>,
    ty: Ty<'ctx>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind<'ctx> {
    Ident(Ident),
    Literal(Literal<'ctx>),
    Binary(BinOp, Box<Expr<'ctx>>, Box<Expr<'ctx>>),
    Unary(UnaryOp, Box<Expr<'ctx>>),

    Let(Ident, Option<TypePath>, Box<Expr<'ctx>>),
    Set(Ident, Box<Expr<'ctx>>),

    FnCall(Ident, Vec<Expr<'ctx>>),
    Block(Vec<Expr<'ctx>>),
    If {
        cond: Box<Expr<'ctx>>,
        // TODO: Maybe make this a Block type?
        then_expr: Box<Expr<'ctx>>,
        else_expr: Option<Box<Expr<'ctx>>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal<'ctx> {
    span: Span,
    kind: LiteralKind,
    ty: Ty<'ctx>,
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
    Land,
    Lor,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}
