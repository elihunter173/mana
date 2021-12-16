use std::fmt::Display;

use crate::{
    // TODO: Merge ast with parse?
    ast::*,
    lex::{Lexer, Token, TokenKind},
};

// TODO: Figure out good error reporting
#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    EndOfStream,
    UnexpectedToken(Token),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Do something better
        write!(f, "{:?}", self)
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'input> {
    lexer: Lexer<'input>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn finished(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
}

impl<'input> Parser<'input> {
    pub fn items(&mut self) -> ParseResult<Vec<Item>> {
        todo!()
    }

    pub fn item(&mut self) -> ParseResult<Item> {
        todo!()
    }

    pub fn fn_def(&mut self) -> ParseResult<Item> {
        todo!()
    }

    pub fn import(&mut self) -> ParseResult<Item> {
        todo!()
    }

    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.expr_at_binding(0)?;
        todo!("Support expressions other than just arithmetic ones")
    }

    pub fn block(&mut self) -> ParseResult<Expr> {
        todo!()
    }

    pub fn if_(&mut self) -> ParseResult<Expr> {
        todo!()
    }

    pub fn fn_call(&mut self) -> ParseResult<Expr> {
        todo!()
    }

    pub fn let_(&mut self) -> ParseResult<Expr> {
        todo!()
    }

    // TODO: Rename this?
    pub fn set(&mut self) -> ParseResult<Expr> {
        let ident_tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        if ident_tok.kind != TokenKind::Ident {
            return Err(ParseError::UnexpectedToken(ident_tok));
        }
        let ident = Ident(self.lexer.source(&ident_tok).to_owned());

        let assign_op = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        let rtn = match assign_op.kind {
            TokenKind::Equals => Ok(Expr::Set(ident, Box::new(self.expr()?))),
            TokenKind::PlusEq => Ok(Expr::Set(
                ident.clone(),
                Box::new(Expr::Binary(
                    BinOp::Add,
                    Box::new(Expr::Ident(ident)),
                    Box::new(self.expr()?),
                )),
            )),
            TokenKind::MinusEq => Ok(Expr::Set(
                ident.clone(),
                Box::new(Expr::Binary(
                    BinOp::Sub,
                    Box::new(Expr::Ident(ident)),
                    Box::new(self.expr()?),
                )),
            )),
            TokenKind::StarEq => Ok(Expr::Set(
                ident.clone(),
                Box::new(Expr::Binary(
                    BinOp::Mul,
                    Box::new(Expr::Ident(ident)),
                    Box::new(self.expr()?),
                )),
            )),
            TokenKind::SlashEq => Ok(Expr::Set(
                ident.clone(),
                Box::new(Expr::Binary(
                    BinOp::Div,
                    Box::new(Expr::Ident(ident)),
                    Box::new(self.expr()?),
                )),
            )),
            _ => Err(ParseError::UnexpectedToken(assign_op)),
        };
        if rtn.is_ok() {
            self.lexer.next();
        }
        rtn
    }

    fn expr_at_binding(&mut self, binding: usize) -> ParseResult<Expr> {
        const OPERATIONS_BY_BINDING: [fn(Token) -> Option<BinOp>; 5] = [
            // Loosest binding
            |token| match token.kind {
                TokenKind::Or => Some(BinOp::Lor),
                _ => None,
            },
            |token| match token.kind {
                TokenKind::And => Some(BinOp::Land),
                _ => None,
            },
            |token| match token.kind {
                TokenKind::Equals => Some(BinOp::Eq),
                TokenKind::BangEquals => Some(BinOp::Neq),
                TokenKind::Lt => Some(BinOp::Lt),
                TokenKind::Leq => Some(BinOp::Leq),
                TokenKind::Gt => Some(BinOp::Gt),
                TokenKind::Geq => Some(BinOp::Geq),
                _ => None,
            },
            |token| match token.kind {
                TokenKind::Plus => Some(BinOp::Add),
                TokenKind::Minus => Some(BinOp::Sub),
                _ => None,
            },
            |token| match token.kind {
                TokenKind::Star => Some(BinOp::Mul),
                TokenKind::Slash => Some(BinOp::Div),
                _ => None,
            },
            // Tightest binding
        ];

        if binding >= OPERATIONS_BY_BINDING.len() {
            return self.atom();
        }

        let mut expr = self.expr_at_binding(binding + 1)?;
        loop {
            // TODO: I want operations at the same level to be ambiguous if they're not defined
            // together
            let op = if let Some(token) = self.lexer.peek() {
                match OPERATIONS_BY_BINDING[binding](token) {
                    Some(op) => op,
                    None => break,
                }
            } else {
                break;
            };
            self.lexer.next();
            let right = self.expr_at_binding(binding + 1)?;
            expr = Expr::Binary(op, Box::new(expr), Box::new(right));
        }

        Ok(expr)
    }

    fn atom(&mut self) -> ParseResult<Expr> {
        match self.lexer.peek() {
            Some(Token {
                kind: TokenKind::LParen,
                ..
            }) => {
                self.lexer.next();
                let expr = self.expr()?;
                match self.lexer.next() {
                    Some(Token {
                        kind: TokenKind::RParen,
                        ..
                    }) => {}
                    Some(token) => return Err(ParseError::UnexpectedToken(token)),
                    None => return Err(ParseError::EndOfStream),
                }
                Ok(expr)
            }
            Some(Token {
                kind: TokenKind::Ident,
                ..
            }) => Ok(Expr::Ident(self.ident()?)),
            _ => Ok(Expr::Literal(self.lit()?)),
        }
    }

    pub fn ident(&mut self) -> ParseResult<Ident> {
        let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        let rtn = match tok.kind {
            TokenKind::Ident => Ok(Ident(self.lexer.source(&tok).to_owned())),
            _ => Err(ParseError::UnexpectedToken(tok)),
        };
        if rtn.is_ok() {
            self.lexer.next();
        }
        rtn
    }

    pub fn type_path(&mut self) -> ParseResult<TypePath> {
        let mut path = vec![self.ident()?];
        while let Some(Token {
            kind: TokenKind::Dot,
            ..
        }) = self.lexer.peek()
        {
            self.lexer.next();
            path.push(self.ident()?);
        }
        Ok(TypePath { path })
    }

    pub fn lit(&mut self) -> ParseResult<Literal> {
        let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        let src = self.lexer.source(&tok);

        let rtn = match tok.kind {
            TokenKind::Int => Ok(Literal::parse_int(src, 10)),
            TokenKind::IntHex => Ok(Literal::parse_int(&src[2..], 16)),
            TokenKind::IntOct => Ok(Literal::parse_int(&src[2..], 8)),
            TokenKind::IntBin => Ok(Literal::parse_int(&src[2..], 2)),
            TokenKind::Float => Ok(Literal::parse_float(src)),
            TokenKind::String => {
                // TODO: Move this to Literal?
                // Unescape quotes
                let val = src[1..src.len() - 1].replace("\\\"", "\"");
                Ok(Literal::String(val))
            }
            TokenKind::True => Ok(Literal::Bool(true)),
            TokenKind::False => Ok(Literal::Bool(false)),
            _ => Err(ParseError::UnexpectedToken(tok)),
        };
        if rtn.is_ok() {
            self.lexer.next();
        }
        rtn
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::*;

    use matches::assert_matches;

    // TODO: Replace this with something cleaner
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

    // These are a macros rather than a function because the return value is really hairy
    fn parser(code: &str) -> Parser {
        Parser::new(Lexer::new(code))
    }

    // TODO: Stop using .into()
    macro_rules! test_literals {
        ($( ($test_name:ident, $code:literal, $val:literal$(,)?) ),* $(,)?) => {$(
            #[test]
            fn $test_name() {
                let mut parser = parser($code);
                let want = Ok($val.into());
                assert_eq!(parser.lit(), want);
                assert!(parser.finished());
            }
        )*};
    }

    test_literals! {
        (bool_true, "true", true),
        (bool_false, "false", false),

        (int, "1", 1),
        (int_zero, "0", 0),
        (int_underscore, "1_000", 1_000),
        (int_hex, "0xDEADbeef", 0xDEADBEEF),
        (int_hex_underscore, "0x__123__abc_", 0x123_abc),
        (int_oct, "0o76543210", 0o76543210),
        (int_oct_underscore, "0o100_644", 0o100_644),
        (int_bin, "0b0101", 0b0101),
        (int_bin_underscore, "0b0101_1111", 0b0101_1111),

        (float, "1.0", 1.0),
        (float_underscore, "1_000.0", 1_000.0),
        (float_e, "1e-06", 1e-06),
        (float_e_cap, "1E-06", 1E-06),
        (float_e_underscore, "1_000.123_456e+3", 1_000.123_456e+3),

        (str_basic, r#""Hello, World!""#, "Hello, World!"),
        (str_escape, r#""The cowboy said \"Howdy!\" and then walked away""#, r#"The cowboy said "Howdy!" and then walked away"#),
    }

    #[test]
    fn type_path() {
        let mut parser = parser("Foo.Bar");
        let want = Ok(TypePath {
            path: vec![Ident("Foo".to_owned()), Ident("Bar".to_owned())],
        });
        assert_eq!(parser.type_path(), want);
        assert!(parser.finished());
    }

    #[test]
    fn logical_precedance() {
        use BinOp::*;
        use Expr::*;
        let b = Box::new;

        let mut parser = parser("(true and false) or true");
        let want = Ok(Binary(
            Lor,
            b(Binary(
                Land,
                b(Literal(true.into())),
                b(Literal(false.into())),
            )),
            b(Literal(true.into())),
        ));
        assert_eq!(parser.expr(), want);
        assert!(parser.finished());
    }

    #[test]
    #[ignore = "I'm not sure this is what I want"]
    fn logical_same_level() {
        let mut parser = parser("true and false or true");
        assert_matches!(parser.expr(), Err(_));
    }

    #[test]
    fn arithmetic_precedance() {
        use BinOp::*;
        use Expr::*;
        let b = Box::new;

        let mut parser = parser("1 + 2 * 3 / (4 - 5)");
        let want = Ok(Binary(
            Add,
            b(Literal(1.into())),
            b(Binary(
                Div,
                b(Binary(Mul, b(Literal(2.into())), b(Literal(3.into())))),
                b(Binary(Sub, b(Literal(4.into())), b(Literal(5.into())))),
            )),
        ));
        assert_eq!(parser.expr(), want);
        assert!(parser.finished());
    }

    /*
        #[test]
        fn test_fn_def() {
            let got = item!(
                "
    fn the_answer() -> UInt {
        42
    }"
            );
            let want = Ok(Item::FnDef {
                name: Ident {
                    name: "the_answer".into(),
                },
                args: vec![],
                return_type: Some(TypePath {
                    path: vec![Ident {
                        name: "UInt".into(),
                    }],
                }),
                body: vec![Expr::Literal(42.into())],
            });
            assert_eq!(got, want);
        }
        */
}
