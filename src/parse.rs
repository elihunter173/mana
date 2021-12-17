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

impl<'input> Parser<'input> {
    pub fn new(code: &'input str) -> Self {
        Self { lexer: Lexer::new(code) }
    }

    pub fn finished(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
}

// Helpers
impl<'input> Parser<'input> {
    fn token(&mut self, kind: TokenKind) -> ParseResult<Token> {
        let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        if tok.kind != kind {
            return Err(ParseError::UnexpectedToken(tok));
        }
        self.lexer.next();
        Ok(tok)
    }

    // TODO: Is this useful?
    fn try_token(&mut self, kind: TokenKind) -> Option<Token> {
        let tok = self.lexer.peek()?;
        if tok.kind != kind {
            return None;
        }
        self.lexer.next();
        Some(tok)
    }

    fn delimited<T>(
        &mut self,
        start: TokenKind,
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
        delimiter: TokenKind,
        end: TokenKind,
    ) -> ParseResult<Vec<T>> {
        self.token(start)?;
        let mut parsed = Vec::new();
        let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        if tok.kind == end {
            self.lexer.next();
        } else {
            // TODO: Make delimited helper
            parsed.push(parser(self)?);
            while self.try_token(delimiter).is_some() {
                // This allows for trailing delimiters
                // TODO: This is really hard to read
                // If the token we're peeking at is what we want
                if self.lexer.peek().filter(|tok| tok.kind == end).is_some() {
                    break;
                }
                parsed.push(parser(self)?);
            }
            self.token(end)?;
        }
        Ok(parsed)
    }
}

impl<'input> Parser<'input> {
    pub fn items(&mut self) -> ParseResult<Vec<Item>> {
        // TODO: Figure out a way to re-use delimited
        let mut items = Vec::new();
        if !self.finished() {
            items.push(self.item()?);
        }
        while self.try_token(TokenKind::Semicolon).is_some() {
            if self.finished() {
                break;
            }
            items.push(self.item()?);
        }
        Ok(items)
    }

    pub fn item(&mut self) -> ParseResult<Item> {
        let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        match tok.kind {
            TokenKind::Import => self.import(),
            TokenKind::Fn => Ok(Item::FnDef(self.fn_def()?)),
            _ => Err(ParseError::UnexpectedToken(tok)),
        }
    }

    pub fn fn_def(&mut self) -> ParseResult<FnDef> {
        self.token(TokenKind::Fn)?;
        let name = self.ident()?;
        let args = self.delimited(
            TokenKind::LParen,
            |parser| {
                let ident = parser.ident()?;
                parser.token(TokenKind::Colon)?;
                let typepath = parser.typepath()?;
                Ok((ident, typepath))
            },
            TokenKind::Comma,
            TokenKind::RParen,
        )?;
        let return_type = if self.try_token(TokenKind::Colon).is_some() {
            Some(self.typepath()?)
        } else {
            None
        };
        let body = self.block()?;
        Ok(FnDef {
            name,
            params: args,
            return_typepath: return_type,
            body,
        })
    }

    pub fn import(&mut self) -> ParseResult<Item> {
        self.token(TokenKind::Import)?;
        let typepath = self.typepath()?;
        Ok(Item::Import(typepath))
    }

    pub fn expr(&mut self) -> ParseResult<Expr> {
        let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        match tok.kind {
            TokenKind::LCurly => Ok(Expr::Block(self.block()?)),
            TokenKind::If => self.if_chain(),
            TokenKind::Let => self.let_(),
            _ => self.expr_at_binding(0),
        }
    }

    pub fn block(&mut self) -> ParseResult<Vec<Expr>> {
        self.delimited(
            TokenKind::LCurly,
            Self::expr,
            TokenKind::Semicolon,
            TokenKind::RCurly,
        )
    }

    pub fn if_chain(&mut self) -> ParseResult<Expr> {
        self.token(TokenKind::If)?;
        let cond = self.expr()?;
        let true_block = self.block()?;
        let false_expr = if self.try_token(TokenKind::Else).is_some() {
            let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
            if tok.kind == TokenKind::If {
                Some(self.if_chain()?)
            } else {
                Some(Expr::Block(self.block()?))
            }
        } else {
            None
        };
        Ok(Expr::If {
            cond: Box::new(cond),
            then_expr: Box::new(Expr::Block(true_block)),
            else_expr: false_expr.map(Box::new),
        })
    }

    pub fn fn_args(&mut self) -> ParseResult<Vec<Expr>> {
        self.delimited(
            TokenKind::LParen,
            Self::expr,
            TokenKind::Comma,
            TokenKind::RParen,
        )
    }

    pub fn let_(&mut self) -> ParseResult<Expr> {
        self.token(TokenKind::Let)?;
        let ident = self.ident()?;
        let typepath = if self.try_token(TokenKind::Colon).is_some() {
            Some(self.typepath()?)
        } else {
            None
        };
        self.token(TokenKind::Equals)?;
        let expr = self.expr()?;

        Ok(Expr::Let(ident, typepath, Box::new(expr)))
    }

    pub fn set(&mut self) -> ParseResult<Expr> {
        let ident = self.ident()?;

        // TODO: This is gross
        let assign_op = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        let rtn = match assign_op.kind {
            TokenKind::Equals => Expr::Set(ident, Box::new(self.expr()?)),
            TokenKind::PlusEq => Expr::Set(
                ident.clone(),
                Box::new(Expr::Binary(
                    BinOp::Add,
                    Box::new(Expr::Ident(ident)),
                    Box::new(self.expr()?),
                )),
            ),
            TokenKind::MinusEq => Expr::Set(
                ident.clone(),
                Box::new(Expr::Binary(
                    BinOp::Sub,
                    Box::new(Expr::Ident(ident)),
                    Box::new(self.expr()?),
                )),
            ),
            TokenKind::StarEq => Expr::Set(
                ident.clone(),
                Box::new(Expr::Binary(
                    BinOp::Mul,
                    Box::new(Expr::Ident(ident)),
                    Box::new(self.expr()?),
                )),
            ),
            TokenKind::SlashEq => Expr::Set(
                ident.clone(),
                Box::new(Expr::Binary(
                    BinOp::Div,
                    Box::new(Expr::Ident(ident)),
                    Box::new(self.expr()?),
                )),
            ),
            _ => return Err(ParseError::UnexpectedToken(assign_op)),
        };
        self.lexer.next();
        Ok(rtn)
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
            Some(Token { kind: TokenKind::LParen, .. }) => {
                self.lexer.next();
                let expr = self.expr()?;
                self.token(TokenKind::RParen)?;
                Ok(expr)
            }
            Some(Token { kind: TokenKind::Ident, .. }) => {
                // Function call or identifier
                let ident = self.ident()?;
                let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
                match tok.kind {
                    TokenKind::LParen => {
                        let args = self.fn_args()?;
                        Ok(Expr::FnCall(ident, args))
                    }
                    TokenKind::Equals => {
                        self.lexer.next();
                        // TODO: Do I want Set to be an expression?
                        let expr = self.expr()?;
                        Ok(Expr::Set(ident, Box::new(expr)))
                    }
                    _ => Ok(Expr::Ident(ident)),
                }
            }
            _ => Ok(Expr::Literal(self.lit()?)),
        }
    }

    pub fn ident(&mut self) -> ParseResult<Ident> {
        let tok = self.token(TokenKind::Ident)?;
        Ok(Ident(self.lexer.source(&tok).to_owned()))
    }

    pub fn typepath(&mut self) -> ParseResult<TypePath> {
        let mut path = vec![self.ident()?];
        while let Some(Token { kind: TokenKind::Dot, .. }) = self.lexer.peek() {
            self.lexer.next();
            path.push(self.ident()?);
        }
        Ok(TypePath { path })
    }

    pub fn lit(&mut self) -> ParseResult<Literal> {
        let tok = self.lexer.peek().ok_or(ParseError::EndOfStream)?;
        let src = self.lexer.source(&tok);

        let literal = match tok.kind {
            TokenKind::Int => Literal::parse_int(src, 10),
            TokenKind::IntHex => Literal::parse_int(&src[2..], 16),
            TokenKind::IntOct => Literal::parse_int(&src[2..], 8),
            TokenKind::IntBin => Literal::parse_int(&src[2..], 2),
            TokenKind::Float => Literal::parse_float(src),
            TokenKind::String => {
                // TODO: Move this to Literal?
                // Unescape quotes
                let val = src[1..src.len() - 1].replace("\\\"", "\"");
                Literal::String(val)
            }
            TokenKind::True => Literal::Bool(true),
            TokenKind::False => Literal::Bool(false),
            _ => return Err(ParseError::UnexpectedToken(tok)),
        };
        self.lexer.next();
        Ok(literal)
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
        Parser::new(code)
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
    fn typepath() {
        let mut parser = parser("Foo.Bar");
        let want = Ok(TypePath {
            path: vec![Ident("Foo".to_owned()), Ident("Bar".to_owned())],
        });
        assert_eq!(parser.typepath(), want);
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

    #[test]
    fn fn_calls() {
        let b = Box::new;

        let mut parser = parser("foo(n, \"bar\") + 1");
        let want = Ok(Expr::Binary(
            BinOp::Add,
            b(Expr::FnCall(
                Ident("foo".to_owned()),
                vec![
                    Expr::Ident(Ident("n".to_owned())),
                    Expr::Literal("bar".into()),
                ],
            )),
            b(Expr::Literal(1.into())),
        ));
        assert_eq!(parser.expr(), want);
        assert!(parser.finished());
    }

    #[test]
    fn test_fn_def() {
        let mut parser = parser(
            "
fn the_answer(): UInt {
    42
}",
        );
        let want = Ok(Item::FnDef {
            name: Ident("the_answer".to_owned()),
            args: vec![],
            return_type: Some(TypePath { path: vec![Ident("UInt".to_owned())] }),
            body: vec![Expr::Literal(42.into())],
        });
        assert_eq!(parser.item(), want);
        assert!(parser.finished());
    }
}
