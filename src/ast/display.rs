use core::fmt::{self, Write};

use super::*;
use crate::intern::SymbolInterner;

impl Module {
    pub fn display<'ctx>(&'ctx self, symbols: &'ctx SymbolInterner) -> impl fmt::Display + 'ctx {
        SexprWrapper { node: self, symbols }
    }
}

struct SexprWrapper<'ctx, T: Sexpr> {
    node: &'ctx T,
    symbols: &'ctx SymbolInterner,
}

impl<T: Sexpr> fmt::Display for SexprWrapper<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut sexpr_writer = SexprWriter {
            nesting: 0,
            first_element: true,
            writer: f,
            symbols: self.symbols,
        };
        sexpr_writer.write(self.node).map_err(|_| fmt::Error)
    }
}

struct SexprWriter<'ctx, W: Write> {
    nesting: u16,
    first_element: bool,
    writer: W,
    symbols: &'ctx SymbolInterner,
}

impl<W: Write> SexprWriter<'_, W> {
    fn write<Elem: Sexpr>(&mut self, elem: &Elem) -> fmt::Result {
        elem.write_into(self)
    }

    fn write_str(&mut self, s: &str) -> fmt::Result {
        if self.first_element {
            write!(self.writer, "{}", s)?;
        } else {
            write!(self.writer, " {}", s)?;
        }
        self.first_element = false;
        Ok(())
    }

    fn list(&mut self, f: impl FnOnce(&mut Self) -> fmt::Result) -> fmt::Result {
        // Begin list
        self.nesting += 1;
        if self.first_element {
            self.writer.write_char('(')?;
        } else {
            self.writer.write_str(" (")?;
        }
        self.first_element = true;

        f(self)?;

        // End list
        self.nesting -= 1;
        self.writer.write_char(')')?;
        self.first_element = false;
        Ok(())
    }
}

trait Sexpr {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result;
}

impl Sexpr for Ident {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        w.write_str(w.symbols.resolve(&self.sym))
    }
}

impl Sexpr for IdentPath {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        let mut repr = String::new();
        if let Some((head_ident, tail)) = self.path.split_first() {
            repr.push_str(w.symbols.resolve(&head_ident.sym));
            for ident in tail {
                repr.push('.');
                repr.push_str(w.symbols.resolve(&ident.sym));
            }
        }
        w.write_str(&repr)
    }
}

impl Sexpr for Module {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        for item in &self.items {
            w.write(item)?;
        }
        Ok(())
    }
}

impl Sexpr for Item {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        match self {
            Item::Def(def) => w.write(def),
            Item::Import(ident_path) => w.list(|w| {
                w.write_str("import")?;
                w.write(ident_path)?;
                Ok(())
            }),
            Item::Error => w.write_str("ERROR"),
        }
    }
}

impl Sexpr for Def {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        w.list(|w| {
            w.write_str("def")?;
            w.write(&self.name)?;
            w.write(&self.value)?;
            Ok(())
        })
    }
}

impl Sexpr for Expr {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        match &self.kind {
            ExprKind::Error => w.write_str("ERROR"),

            ExprKind::Ident(ident) => w.write(ident),
            ExprKind::Literal(lit) => w.write(lit),
            ExprKind::Binary(op, lhs, rhs) => w.list(|w| {
                w.write(op)?;
                w.write(lhs.as_ref())?;
                w.write(rhs.as_ref())?;
                Ok(())
            }),
            ExprKind::Unary(op, expr) => w.list(|w| {
                w.write(op)?;
                w.write(expr.as_ref())?;
                Ok(())
            }),
            ExprKind::Index(expr, index_expr) => w.list(|w| {
                w.write_str("[]")?;
                w.write(expr.as_ref())?;
                w.write(index_expr.as_ref())?;
                Ok(())
            }),
            ExprKind::Access(expr, ident) => w.list(|w| {
                w.write_str(".")?;
                w.write(expr.as_ref())?;
                w.write(ident)?;
                Ok(())
            }),
            ExprKind::Let(ident, typepath, expr) => w.list(|w| {
                w.write_str("let")?;
                w.write(ident)?;
                if let Some(typepath) = typepath {
                    w.write(typepath)?;
                } else {
                    w.write_str("INFER_TYPE")?;
                }
                w.write(expr.as_ref())?;
                Ok(())
            }),
            ExprKind::Set(ident, expr) => w.list(|w| {
                w.write_str("set")?;
                w.write(ident)?;
                w.write(expr.as_ref())?;
                Ok(())
            }),
            ExprKind::Loop(body) => w.list(|w| {
                w.write_str("loop")?;
                w.write(body.as_ref())?;
                Ok(())
            }),
            ExprKind::Break(expr) => w.list(|w| {
                w.write_str("break")?;
                if let Some(expr) = expr {
                    w.write(expr.as_ref())?;
                } else {
                    w.write_str("()")?;
                }
                Ok(())
            }),
            ExprKind::Continue(expr) => w.list(|w| {
                w.write_str("continue")?;
                if let Some(expr) = expr {
                    w.write(expr.as_ref())?;
                } else {
                    w.write_str("()")?;
                }
                Ok(())
            }),
            ExprKind::Return(expr) => w.list(|w| {
                w.write_str("return")?;
                if let Some(expr) = expr {
                    w.write(expr.as_ref())?;
                } else {
                    w.write_str("()")?;
                }
                Ok(())
            }),
            ExprKind::FnCall(ident, args) => w.list(|w| {
                w.write(ident)?;
                for expr in args {
                    w.write(expr)?;
                }
                Ok(())
            }),
            ExprKind::Block(exprs) => w.list(|w| {
                for expr in exprs {
                    w.write(expr)?;
                }
                Ok(())
            }),
            ExprKind::If { cond, then_expr, else_expr } => w.list(|w| {
                w.write_str("if")?;
                w.write(cond.as_ref())?;
                w.write(then_expr.as_ref())?;
                if let Some(expr) = else_expr {
                    w.write(expr.as_ref())?;
                } else {
                    w.write_str("()")?;
                }
                Ok(())
            }),
        }
    }
}

impl Sexpr for LiteralKind {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        match self {
            LiteralKind::Bool(val) => w.write_str(match val {
                true => "true",
                false => "false",
            }),
            LiteralKind::Int(val) => {
                let s = val.to_string();
                w.write_str(&s)
            }
            LiteralKind::Float(sym) => w.write_str(w.symbols.resolve(sym)),
            LiteralKind::String(sym) => {
                let s = format!("\"{}\"", w.symbols.resolve(sym));
                w.write_str(&s)
            }
            LiteralKind::Fn(fn_) => fn_.write_into(w),
        }
    }
}

impl Sexpr for Fn {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        w.list(|w| {
            w.write_str("fn")?;

            w.list(|w| {
                for (ident, typepath) in &self.params {
                    w.list(|w| {
                        w.write(ident)?;
                        w.write(typepath)?;
                        Ok(())
                    })?;
                }
                Ok(())
            })?;

            if let Some(typepath) = &self.return_typepath {
                w.write(typepath)?;
            } else {
                w.write_str("()")?;
            }

            w.list(|w| {
                for expr in &self.body {
                    w.write(expr)?;
                }
                Ok(())
            })?;

            Ok(())
        })
    }
}

impl Sexpr for BinOp {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        w.write_str(match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Rem => "%",
            BinOp::Band => "&",
            BinOp::Bor => "|",
            BinOp::Bxor => "^",

            BinOp::Eq => "==",
            BinOp::Neq => "!=",

            BinOp::Land => "and",
            BinOp::Lor => "or",

            BinOp::Lt => "<",
            BinOp::Leq => "<=",
            BinOp::Gt => ">",
            BinOp::Geq => ">=",
        })
    }
}

impl Sexpr for UnaryOp {
    fn write_into<W: Write>(&self, w: &mut SexprWriter<W>) -> fmt::Result {
        w.write_str(match self {
            UnaryOp::Neg => "-",
            UnaryOp::Lnot => "not",
            UnaryOp::Bnot => "~",
        })
    }
}
