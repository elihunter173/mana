use std::fmt::Display;

use crate::{
    ast::*,
    lex::{Lexer, Token, TokenKind},
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: (usize, usize),
}

// TODO: Figure out good error reporting
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ParseErrorKind {
    UnexpectedEOF,
    UnexpectedToken(Token),
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'input> {
    code: &'input str,
    lexer: Lexer<'input>,
}

impl<'input> Parser<'input> {
    pub fn new(code: &'input str) -> Self {
        Self { code, lexer: Lexer::new(code) }
    }

    pub fn finished(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
}

// Helpers
impl<'input> Parser<'input> {
    fn try_peek(&mut self) -> ParseResult<Token> {
        self.lexer.peek().ok_or_else(|| ParseError {
            kind: ParseErrorKind::UnexpectedEOF,
            span: (self.code.len() - 1, self.code.len() - 1),
        })
    }

    fn token(&mut self, kind: TokenKind) -> ParseResult<Token> {
        let tok = self.try_peek()?;
        if tok.kind != kind {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(tok),
                span: tok.span,
            });
        }
        self.lexer.next();
        Ok(tok)
    }

    // TODO: Is this useful?
    fn maybe_token(&mut self, kind: TokenKind) -> Option<Token> {
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
    ) -> ParseResult<(Vec<T>, Span)> {
        let start_tok = self.token(start)?;
        let mut parsed = Vec::new();
        let tok = self.try_peek()?;
        let end_tok = if tok.kind == end {
            self.lexer.next();
            tok
        } else {
            // TODO: Make delimited helper
            parsed.push(parser(self)?);
            while self.maybe_token(delimiter).is_some() {
                // This allows for trailing delimiters
                // TODO: This is really hard to read
                // If the token we're peeking at is what we want
                if self.lexer.peek().filter(|tok| tok.kind == end).is_some() {
                    break;
                }
                parsed.push(parser(self)?);
            }
            self.token(end)?
        };
        Ok((parsed, (start_tok.span.0, end_tok.span.1)))
    }
}

impl<'input> Parser<'input> {
    pub fn items(&mut self) -> ParseResult<Vec<Item>> {
        // TODO: Figure out a way to re-use delimited
        let mut items = Vec::new();
        if !self.finished() {
            items.push(self.item()?);
        }
        while self.maybe_token(TokenKind::Semicolon).is_some() {
            if self.finished() {
                break;
            }
            items.push(self.item()?);
        }
        Ok(items)
    }

    pub fn item(&mut self) -> ParseResult<Item> {
        let tok = self.try_peek()?;
        match tok.kind {
            TokenKind::Import => self.import(),
            TokenKind::Fn => Ok(Item::FnDef(self.fn_def()?)),
            _ => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(tok),
                span: tok.span,
            }),
        }
    }

    pub fn fn_def(&mut self) -> ParseResult<FnDef> {
        self.token(TokenKind::Fn)?;
        let name = self.ident()?;
        let (args, _span) = self.delimited(
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
        let return_type = if self.maybe_token(TokenKind::Colon).is_some() {
            Some(self.typepath()?)
        } else {
            None
        };
        let (body, _span) = self.block()?;
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
        match self.try_peek()?.kind {
            TokenKind::LCurly => {
                let (block, span) = self.block()?;
                Ok(Expr { kind: ExprKind::Block(block), span })
            }
            TokenKind::If => self.if_chain(),
            TokenKind::Let => self.let_(),
            _ => self.expr_at_binding(0),
        }
    }

    pub fn block(&mut self) -> ParseResult<(Vec<Expr>, Span)> {
        self.delimited(
            TokenKind::LCurly,
            Self::expr,
            TokenKind::Semicolon,
            TokenKind::RCurly,
        )
    }

    pub fn if_chain(&mut self) -> ParseResult<Expr> {
        let if_ = self.token(TokenKind::If)?;
        let cond = self.expr()?;
        let (true_block, true_block_span) = self.block()?;

        let false_expr = if self.maybe_token(TokenKind::Else).is_some() {
            let tok = self.try_peek()?;
            if tok.kind == TokenKind::If {
                Some(self.if_chain()?)
            } else {
                let (false_block, false_block_span) = self.block()?;
                Some(Expr {
                    kind: ExprKind::Block(false_block),
                    span: false_block_span,
                })
            }
        } else {
            None
        };

        let span = (
            if_.span.0,
            false_expr
                .as_ref()
                .map(|f| f.span.1)
                .unwrap_or(true_block_span.1),
        );
        Ok(Expr {
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_expr: Box::new(Expr {
                    kind: ExprKind::Block(true_block),
                    span: true_block_span,
                }),
                else_expr: false_expr.map(Box::new),
            },
            span,
        })
    }

    pub fn fn_args(&mut self) -> ParseResult<(Vec<Expr>, Span)> {
        self.delimited(
            TokenKind::LParen,
            Self::expr,
            TokenKind::Comma,
            TokenKind::RParen,
        )
    }

    pub fn let_(&mut self) -> ParseResult<Expr> {
        let let_ = self.token(TokenKind::Let)?;
        let ident = self.ident()?;
        let typepath = if self.maybe_token(TokenKind::Colon).is_some() {
            Some(self.typepath()?)
        } else {
            None
        };
        self.token(TokenKind::Equals)?;
        let expr = self.expr()?;

        let span = (let_.span.0, expr.span.1);
        Ok(Expr {
            kind: ExprKind::Let(ident, typepath, Box::new(expr)),
            span,
        })
    }

    pub fn set(&mut self) -> ParseResult<Expr> {
        let ident = self.ident()?;

        // TODO: This is gross
        let assign_op = self.try_peek()?;
        let op = match assign_op.kind {
            TokenKind::Equals => None,
            TokenKind::PlusEq => Some(BinOp::Add),
            TokenKind::MinusEq => Some(BinOp::Sub),
            TokenKind::StarEq => Some(BinOp::Mul),
            TokenKind::SlashEq => Some(BinOp::Div),
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(assign_op),
                    span: (self.code.len() - 1, self.code.len()),
                })
            }
        };
        self.lexer.next();
        let expr = self.expr()?;

        let assigned_expr = if let Some(op) = op {
            let span = expr.span;
            Expr {
                kind: ExprKind::Binary(
                    op,
                    Box::new(Expr {
                        kind: ExprKind::Ident(ident.clone()),
                        span: ident.span,
                    }),
                    Box::new(expr),
                ),
                span,
            }
        } else {
            expr
        };

        let span = (ident.span.0, assigned_expr.span.1);
        Ok(Expr {
            kind: ExprKind::Set(ident, Box::new(assigned_expr)),
            span,
        })
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
                TokenKind::DoubleEquals => Some(BinOp::Eq),
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
            let span = (expr.span.0, right.span.1);
            expr = Expr {
                kind: ExprKind::Binary(op, Box::new(expr), Box::new(right)),
                span,
            };
        }

        Ok(expr)
    }

    fn atom(&mut self) -> ParseResult<Expr> {
        match self.try_peek()?.kind {
            TokenKind::LParen => {
                self.lexer.next();
                let expr = self.expr()?;
                self.token(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::Ident => {
                // Function call or identifier
                let ident = self.ident()?;
                let tok = self.try_peek()?;
                match tok.kind {
                    TokenKind::LParen => {
                        let (args, span) = self.fn_args()?;
                        Ok(Expr {
                            span: (ident.span.0, span.1),
                            kind: ExprKind::FnCall(ident, args),
                        })
                    }
                    TokenKind::Equals => {
                        self.lexer.next();
                        // TODO: Do I want Set to be an expression?
                        let expr = self.expr()?;
                        Ok(Expr {
                            span: (ident.span.0, expr.span.1),
                            kind: ExprKind::Set(ident, Box::new(expr)),
                        })
                    }
                    _ => Ok(Expr {
                        span: ident.span,
                        kind: ExprKind::Ident(ident),
                    }),
                }
            }
            _ => {
                let lit = self.lit()?;
                Ok(Expr {
                    span: lit.span,
                    kind: ExprKind::Literal(lit),
                })
            }
        }
    }

    pub fn ident(&mut self) -> ParseResult<Ident> {
        let tok = self.token(TokenKind::Ident)?;
        Ok(Ident {
            name: self.lexer.source(&tok).to_owned(),
            span: tok.span,
        })
    }

    pub fn typepath(&mut self) -> ParseResult<TypePath> {
        let head = self.ident()?;
        let mut span = head.span;
        let mut path = vec![head];
        while let Some(Token { kind: TokenKind::Dot, .. }) = self.lexer.peek() {
            self.lexer.next();
            let next = self.ident()?;
            span.1 = next.span.1;
            path.push(next);
        }
        Ok(TypePath { path, span })
    }

    pub fn lit(&mut self) -> ParseResult<Literal> {
        let tok = self.try_peek()?;
        let src = self.lexer.source(&tok);

        let kind = match tok.kind {
            TokenKind::Int => LiteralKind::parse_int(src, 10),
            TokenKind::IntHex => LiteralKind::parse_int(&src[2..], 16),
            TokenKind::IntOct => LiteralKind::parse_int(&src[2..], 8),
            TokenKind::IntBin => LiteralKind::parse_int(&src[2..], 2),
            TokenKind::Float => LiteralKind::parse_float(src),
            TokenKind::String => {
                // TODO: Move this to Literal?
                // Unescape quotes
                let val = src[1..src.len() - 1].replace("\\\"", "\"");
                LiteralKind::String(val)
            }
            TokenKind::True => LiteralKind::Bool(true),
            TokenKind::False => LiteralKind::Bool(false),
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(tok),
                    span: tok.span,
                });
            }
        };
        self.lexer.next();
        Ok(Literal { kind, span: tok.span })
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
        let b = Box::new;

        let mut parser = parser("(true and false) or true");
        let want = Ok(Expr {
            kind: ExprKind::Binary(
                BinOp::Lor,
                b(Expr {
                    kind: ExprKind::Binary(
                        BinOp::Land,
                        b(Expr {
                            kind: ExprKind::Literal(true.into()),
                            span: (1, 5),
                        }),
                        b(ExprKind::Literal(false.into())),
                    ),
                    span: (0, 16),
                }),
                b(ExprKind::Literal(true.into())),
            ),
            span: (0, 18),
        });
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
        let want = Ok(Item::FnDef(FnDef {
            name: Ident("the_answer".to_owned()),
            params: vec![],
            return_typepath: Some(TypePath { path: vec![Ident("UInt".to_owned())] }),
            body: vec![Expr::Literal(42.into())],
        }));
        assert_eq!(parser.item(), want);
        assert!(parser.finished());
    }
}
