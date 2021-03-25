use bumpalo::{boxed::Box, collections::Vec};

#[derive(Debug, PartialEq)]
pub enum ManaliType {
    Bool(bool),
    Number(f64),
    String(String),
}

type Literal = ManaliType;

impl From<bool> for ManaliType {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<f64> for ManaliType {
    fn from(v: f64) -> Self {
        Self::Number(v)
    }
}

impl From<&str> for ManaliType {
    fn from(v: &str) -> Self {
        Self::String(v.to_owned())
    }
}

type BoxExpr<'ast> = Box<'ast, Expr<'ast>>;

#[derive(Debug, PartialEq)]
pub struct Ident<'ast> {
    pub name: &'ast str,
}

#[derive(Debug)]
pub struct TypePath<'ast> {
    pub paths: Vec<'ast, Ident<'ast>>,
}

pub type Block<'ast> = Vec<'ast, BoxExpr<'ast>>;

// Is this a good way to do things?
// pub struct Expr {
//   typ: Type,
//   kind: ExprKind,
// }

#[derive(Debug, PartialEq)]
pub enum Expr<'ast> {
    Ident(Ident<'ast>),
    Literal(ManaliType),
    Binary(BinOp, BoxExpr<'ast>, BoxExpr<'ast>),
    Unary(UnaryOp, BoxExpr<'ast>),

    Let(Ident<'ast>, BoxExpr<'ast>),
    Set(Ident<'ast>, BoxExpr<'ast>),

    FnCall(Ident<'ast>, Vec<'ast, Expr<'ast>>),
    Block(Block<'ast>),
    If {
        cond: BoxExpr<'ast>,
        true_expr: BoxExpr<'ast>,
        false_expr: Option<BoxExpr<'ast>>,
    },
}

impl<'ast> Expr<'ast> {
    pub fn apply_children<F>(&self, mut f: F)
    where
        F: FnMut(&Expr),
    {
        use Expr::*;
        match self {
            Ident(_) | Literal(_) => {}
            Binary(_, l, r) => {
                f(l);
                f(r);
            }
            Unary(_, x) | Let(_, x) | Set(_, x) => {
                f(x);
            }
            If {
                cond: _,
                true_expr,
                false_expr,
            } => {
                f(true_expr);
                if let Some(false_expr) = false_expr {
                    f(false_expr);
                }
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinOp {
    Mul,
    Div,
    Add,
    Sub,
    Eq,
    Neq,
    Land,
    Lor,
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Neg,
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use super::*;
    use crate::grammar::*;
    use BinOp::*;
    use Expr::*;

    macro_rules! assert_matches {
        ($expression:expr, $($pattern:tt)+) => {
            match $expression {
                $($pattern)+ => (),
                ref e => panic!("assertion failed: `{:?}` does not match `{}`", e, stringify!($($pattern)+)),
            }
        }
    }

    macro_rules! test_literals {
        ($( ($test_name:ident, $code:literal, $val:literal$(,)?) ),* $(,)?) => {$(
            #[test]
            fn $test_name() {
                let alloc = Bump::new();
                let got = LiteralParser::new().parse(&alloc, $code);
                let want = Ok(Literal($val.into()));
                assert_eq!(got, want);
            }
        )*};
    }

    test_literals! {
        (bool_true, "true", true),
        (bool_false, "false", false),

        (num_int, "1", 1.0),
        (num_float, "1.0", 1.0),
        (num_pos_zero, "0", 0.0),

        (str_basic, r#""Hello, World!""#, "Hello, World!"),
        (str_escape, r#""The cowboy said \"Howdy!\" and then walked away""#, r#"The cowboy said "Howdy!" and then walked away"#),
    }

    #[test]
    fn logical_precedance() {
        let alloc = Bump::new();
        let b = |v| Box::new_in(v, &alloc);

        let got = ExprParser::new().parse(&alloc, "(true and false) or true");
        let want = Ok(Binary(
            Lor,
            b(Binary(
                Land,
                b(Literal(true.into())),
                b(Literal(false.into())),
            )),
            b(Literal(true.into())),
        ));
        assert_eq!(got, want);
    }

    #[test]
    fn logical_same_level() {
        let alloc = Bump::new();
        let got = ExprParser::new().parse(&alloc, "true and false or true");
        assert_matches!(got, Err(_));
    }

    #[test]
    fn arithmetic_precedance() {
        let alloc = Bump::new();
        let b = |v| Box::new_in(v, &alloc);

        let got = ExprParser::new().parse(&alloc, "1 + 2 * 3 / (4 - 5)");
        let want = Ok(Binary(
            Add,
            b(Literal(1.0.into())),
            b(Binary(
                Div,
                b(Binary(Mul, b(Literal(2.0.into())), b(Literal(3.0.into())))),
                b(Binary(Sub, b(Literal(4.0.into())), b(Literal(5.0.into())))),
            )),
        ));
        assert_eq!(got, want);
    }
}
