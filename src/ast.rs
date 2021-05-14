use std::str::FromStr;

use num::{BigInt, Zero};

#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool),
    Int(BigInt),
    Float(f64),
    String(String),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(s), Self::Bool(o)) => s == o,
            (Self::Int(s), Self::Int(o)) => s == o,
            (Self::String(s), Self::String(o)) => s == o,
            (Self::Float(s), Self::Float(o)) => (s.is_nan() && o.is_nan()) || s == o,
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Literal {
    pub fn parse_int(digits: &str, radix: u32) -> Self {
        // TODO: Maybe optimize this?
        let mut num = BigInt::zero();
        for b in digits.bytes() {
            let d = match b {
                b'0'..=b'9' => b - b'0',
                b'a'..=b'z' => b - b'a' + 10,
                b'A'..=b'Z' => b - b'A' + 10,
                b'_' => continue,
                _ => panic!("unexpected digit {:?}", b),
            };
            num *= radix;
            num += d;
        }

        Self::Int(num)
    }

    pub fn parse_float(input: &str) -> Self {
        // TODO: Use better parsing logic
        let input = input.replace("_", "");
        Literal::Float(f64::from_str(&input).unwrap())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct TypePath {
    pub paths: Vec<Ident>,
}

// Is this a good way to do things?
// pub struct Expr {
//   typ: Type,
//   kind: ExprKind,
// }

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),

    Let(Ident, Box<Expr>),
    Set(Ident, Box<Expr>),

    FnCall(Ident, Vec<Expr>),
    Block(Vec<Expr>),
    If {
        cond: Box<Expr>,
        true_expr: Box<Expr>,
        false_expr: Option<Box<Expr>>,
    },
}

impl Expr {
    // TODO: Find a way to easily iterate over the children
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

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Neg,
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        grammar::*,
        lexer::{self, Lexer},
    };
    use lalrpop_util::ParseError;

    use matches::assert_matches;

    macro_rules! literal_from {
        ( $( ($variant:ident $ty:ty) )* ) => {$(
            impl From<$ty> for Literal {
                fn from(v: $ty) -> Self {
                    Self::$variant(v.into())
                }
            }
        )*};
    }

    literal_from!(
        (Bool bool)
        (Int isize)
        (Float f64)
        (String &str)
    );

    // This is a macro rather than a function because the return value is really hairy
    macro_rules! expr {
        ($code:literal) => {
            ExprParser::new().parse($code, Lexer::new($code))
        };
    }

    macro_rules! test_literals {
        ($( ($test_name:ident, $code:literal, $val:literal$(,)?) ),* $(,)?) => {$(
            #[test]
            fn $test_name() {
                let code = $code;
                let got = LiteralParser::new().parse(code, Lexer::new(code));
                let want = Ok(Expr::Literal($val.into()));
                assert_eq!(got, want);
            }
        )*};
    }

    test_literals! {
        (bool_true, "true", true),
        (bool_false, "false", false),

        // TODO: These tests are failing because the lexer is failing
        // (int, "1", 1),
        // (int_zero, "0", 0),
        // (int_underscore, "1_000", 1_000),
        // (int_hex, "0xDEADbeef", 0xDEADBEEF),
        // (int_hex_underscore, "0x__123__abc_", 0x123_abc),
        // (int_oct, "0o76543210", 0o76543210),
        // (int_oct_underscore, "0o100_644", 0o100_644),
        // (int_bin, "0b0101", 0b0101),
        // (int_bin_underscore, "0b0101_1111", 0b0101_1111),

        // (float, "1.0", 1.0),
        // (float_underscore, "1_000.0", 1_000.0),
        // (float_e, "1e-06", 1e-06),
        // (float_e_cap, "1E-06", 1E-06),
        // (float_e_underscore, "1_000.123_456e+3", 1_000.123_456e+3),

        (str_basic, r#""Hello, World!""#, "Hello, World!"),
        (str_escape, r#""The cowboy said \"Howdy!\" and then walked away""#, r#"The cowboy said "Howdy!" and then walked away"#),
    }

    #[test]
    fn logical_precedance() {
        use BinOp::*;
        use Expr::*;
        let b = Box::new;

        let got = expr!("(true and false) or true");
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
        let got = expr!("true and false or true");
        assert_matches!(got, Err(_));
    }

    #[test]
    fn arithmetic_precedance() {
        use BinOp::*;
        use Expr::*;
        let b = Box::new;

        let got = expr!("1 + 2 * 3 / (4 - 5)");
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
