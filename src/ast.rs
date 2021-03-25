use bumpalo::{boxed::Box, collections::Vec};
use num::{Num, BigInt};

#[derive(Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    // TODO: Maybe use i128 since we only support up to (U)Int64?
    Int(BigInt),
    Float(f64),
    String(String),
}

impl Literal {
    pub fn parse_int(digits: &str, radix: u32) -> Self {
        Self::Int(BigInt::from_str_radix(digits, radix).unwrap())
    }
}

type BoxExpr<'ast> = Box<'ast, Expr<'ast>>;

macro_rules! literal_from {
    ( $( ($variant:ident $ty:ty) )* ) => {$(
        impl From<$ty> for Literal {
            fn from(v: $ty) -> Self {
                Self::$variant(v.into())
            }
        }
    )*};
}

// TODO: Remove this and just improve tests
literal_from!(
    (Bool bool)
    (Int isize)
    (Float f64)
    (String &str)
);

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
    Literal(Literal),
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
    // TODO: Find a better way to do this
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

        (int, "1", 1),
        (int_zero, "0", 0),
        (int_underscore, "1_000", 1),
        (int_hex, "0xDEADbeef", 0xDEADBEEF),
        (int_hex_underscore, "0x__123__abc_", 0x123_abc),
        (int_oct, "0o76543210", 0o76543210),
        (int_oct_underscore, "0o100_644", 0o100_644),
        (int_bin, "0b0101", 0b0101),
        (int_bin_underscore, "0b0101_1111", 0b0101_1111),

        (float, "1.0", 1.0),

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
            b(Literal(1.into())),
            b(Binary(
                Div,
                b(Binary(Mul, b(Literal(2.into())), b(Literal(3.into())))),
                b(Binary(Sub, b(Literal(4.into())), b(Literal(5.into())))),
            )),
        ));
        assert_eq!(got, want);
    }
}
