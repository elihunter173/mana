use crate::{
    ast::{
        lex::{Lexer, Token, TokenKind},
        *,
    },
    intern::SymbolInterner,
};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: (usize, usize),
}

// TODO: Improve error granularity and error reporting
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ParseErrorKind {
    UnexpectedEOF,
    UnexpectedToken(Token),
}

type ParseResult<T> = Result<T, ParseError>;

// TODO: Collect multiple errors
pub struct Parser<'input> {
    code: &'input str,
    lexer: Lexer<'input>,
    symbols: &'input mut SymbolInterner,
}

impl<'input> Parser<'input> {
    pub fn new(code: &'input str, symbols: &'input mut SymbolInterner) -> Self {
        Self {
            code,
            lexer: Lexer::new(code),
            symbols,
        }
    }

    pub fn finished(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
}

// Helpers
impl<'input> Parser<'input> {
    fn try_peek(&mut self) -> ParseResult<Token> {
        self.lexer.peek().ok_or(ParseError {
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
            parsed.push(parser(self)?);
            while self.maybe_token(delimiter).is_some() {
                // This allows for trailing delimiters
                // If the token we're peeking at is what we want. Try to rewrite this if possible
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
    pub fn module(&mut self) -> ParseResult<Module> {
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
        Ok(Module { items })
    }

    fn item(&mut self) -> ParseResult<Item> {
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

    fn fn_def(&mut self) -> ParseResult<FnDef> {
        let start = self.token(TokenKind::Fn)?.span.0;
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
        let (body, (_start, end)) = self.block()?;
        Ok(FnDef {
            name,
            params: args,
            return_typepath: return_type,
            body,
            span: (start, end),
        })
    }

    fn import(&mut self) -> ParseResult<Item> {
        self.token(TokenKind::Import)?;
        let typepath = self.typepath()?;
        Ok(Item::Import(typepath))
    }

    fn expr(&mut self) -> ParseResult<Expr> {
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

    fn block(&mut self) -> ParseResult<(Vec<Expr>, Span)> {
        self.delimited(
            TokenKind::LCurly,
            Self::expr,
            TokenKind::Semicolon,
            TokenKind::RCurly,
        )
    }

    fn if_chain(&mut self) -> ParseResult<Expr> {
        let if_ = self.token(TokenKind::If)?;
        let cond = self.expr()?;
        let (then_block, true_block_span) = self.block()?;

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
                    kind: ExprKind::Block(then_block),
                    span: true_block_span,
                }),
                else_expr: false_expr.map(Box::new),
            },
            span,
        })
    }

    fn fn_args(&mut self) -> ParseResult<(Vec<Expr>, Span)> {
        self.delimited(
            TokenKind::LParen,
            Self::expr,
            TokenKind::Comma,
            TokenKind::RParen,
        )
    }

    fn let_(&mut self) -> ParseResult<Expr> {
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

    fn set(&mut self) -> ParseResult<Expr> {
        let ident = self.ident()?;

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
                        kind: ExprKind::Ident(ident),
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
        while let Some(token) = self.lexer.peek() {
            // TODO: I want operations at the same level to be ambiguous if they're not defined
            // together
            let op = if let Some(op) = OPERATIONS_BY_BINDING[binding](token) {
                op
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
                let lparen = self.lexer.next().unwrap();
                let expr = self.expr()?;
                let rparen = self.token(TokenKind::RParen)?;
                Ok(Expr {
                    span: (lparen.span.0, rparen.span.1),
                    ..expr
                })
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

    fn ident(&mut self) -> ParseResult<Ident> {
        let tok = self.token(TokenKind::Ident)?;
        Ok(Ident {
            sym: self.symbols.get_or_intern(self.lexer.source(&tok)),
            span: tok.span,
        })
    }

    fn typepath(&mut self) -> ParseResult<IdentPath> {
        let head = self.ident()?;
        let mut span = head.span;
        let mut path = vec![head];
        while let Some(_dot) = self.maybe_token(TokenKind::Dot) {
            let next = self.ident()?;
            span.1 = next.span.1;
            path.push(next);
        }
        Ok(IdentPath { path, span })
    }

    fn lit(&mut self) -> ParseResult<Literal> {
        let tok = self.try_peek()?;
        let src = self.lexer.source(&tok);

        let kind = match tok.kind {
            TokenKind::Int => LiteralKind::Int(parse_int(src, 10)),
            TokenKind::IntHex => LiteralKind::Int(parse_int(&src[2..], 16)),
            TokenKind::IntOct => LiteralKind::Int(parse_int(&src[2..], 8)),
            TokenKind::IntBin => LiteralKind::Int(parse_int(&src[2..], 2)),
            TokenKind::Float => LiteralKind::Float(self.symbols.get_or_intern(&parse_float(src))),
            TokenKind::String => {
                LiteralKind::String(self.symbols.get_or_intern(&parse_string(src)))
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

// TODO: Return Results for these rather than panicking
fn parse_int(digits: &str, radix: u32) -> u128 {
    let mut num = 0;
    for b in digits.bytes() {
        let d = match b {
            b'0'..=b'9' => b - b'0',
            b'a'..=b'z' => b - b'a' + 10,
            b'A'..=b'Z' => b - b'A' + 10,
            b'_' => continue,
            _ => panic!("unexpected digit {:?}", b),
        };
        num *= radix as u128;
        num += d as u128;
    }

    num
}

// TODO: Actually use parse_float
fn parse_float(input: &str) -> String {
    // TODO: Use better parsing logic
    input.replace('_', "")
}

fn parse_string(input: &str) -> String {
    // Unescape quotes
    input[1..input.len() - 1].replace("\\\"", "\"")
}

#[cfg(test)]
mod tests {
    use crate::intern::Symbol;

    use super::*;

    fn run_parser<T: 'static>(
        code: &str,
        node: impl FnOnce(&mut Parser) -> T,
    ) -> (T, impl FnMut(&str) -> Symbol) {
        let mut symbols = SymbolInterner::new();
        let mut parser = Parser::new(code, &mut symbols);
        let rtn = node(&mut parser);
        (rtn, move |string: &str| symbols.get_or_intern(string))
    }

    #[test]
    fn typepath() {
        let (got, mut sym) = run_parser("Foo.Bar", |p| p.typepath());

        let want = Ok(IdentPath {
            path: vec![
                Ident { sym: sym("Foo"), span: (0, 3) },
                Ident { sym: sym("Bar"), span: (4, 7) },
            ],
            span: (0, 7),
        });
        assert_eq!(got, want);
    }

    #[test]
    fn logical_precedance() {
        #[track_caller]
        fn expect_op(op: BinOp, expr: &Expr) -> (&Expr, &Expr) {
            match &expr.kind {
                ExprKind::Binary(actual_op, left, right) if *actual_op == op => {
                    (left.as_ref(), right.as_ref())
                }
                _ => panic!("expected {:?}", op),
            }
        }

        let (expr, _) = run_parser("(true and false) or true", |p| p.expr());
        let expr = expr.expect("parse failed");
        assert_eq!(expr.span, (0, 24));
        let (left, _right) = expect_op(BinOp::Lor, &expr);
        assert_eq!(left.span, (0, 16));
        expect_op(BinOp::Land, &left);
    }

    #[test]
    #[ignore = "I'm not sure this is what I want"]
    fn logical_same_level() {
        let (got, _) = run_parser("true and false or true", |p| p.expr());
        assert!(got.is_err());
    }

    #[test]
    fn arithmetic_precedance() {
        #[track_caller]
        fn expect_op(op: BinOp, expr: &Expr) -> (&Expr, &Expr) {
            match &expr.kind {
                ExprKind::Binary(actual_op, left, right) if *actual_op == op => {
                    (left.as_ref(), right.as_ref())
                }
                _ => panic!("expected {:?}, expr: {:?}", op, expr),
            }
        }

        let (expr, _) = run_parser("1 + 2 * 3 / (4 - 5)", |p| p.expr());
        let expr = expr.expect("parse failed");
        assert_eq!(expr.span, (0, 19));
        let (_left, right) = expect_op(BinOp::Add, &expr);
        assert_eq!(right.span, (4, 19));
        let (left, right) = expect_op(BinOp::Div, &right);
        expect_op(BinOp::Mul, left);
        assert_eq!(right.span, (12, 19));
        expect_op(BinOp::Sub, &right);
    }

    #[test]
    fn fn_calls() {
        let (got, mut sym) = run_parser("foo(n, \"bar\") + 1", |p| p.expr());

        let want = Ok(Expr {
            span: (0, 17),
            kind: ExprKind::Binary(
                BinOp::Add,
                Box::new(Expr {
                    span: (0, 13),
                    kind: ExprKind::FnCall(
                        Ident { sym: sym("foo"), span: (0, 3) },
                        vec![
                            Expr {
                                span: (4, 5),
                                kind: ExprKind::Ident(Ident { sym: sym("n"), span: (4, 5) }),
                            },
                            Expr {
                                span: (7, 12),
                                kind: ExprKind::Literal(Literal {
                                    span: (7, 12),
                                    kind: LiteralKind::String(sym("bar")),
                                }),
                            },
                        ],
                    ),
                }),
                Box::new(Expr {
                    span: (16, 17),
                    kind: ExprKind::Literal(Literal {
                        span: (16, 17),
                        kind: LiteralKind::Int(1),
                    }),
                }),
            ),
        });
        assert_eq!(got, want);
    }

    #[test]
    fn test_fn_def() {
        let code = "
fn the_answer(): UInt {
    42
}";
        let (got, mut sym) = run_parser(code, |p| p.item());

        let want = Ok(Item::FnDef(FnDef {
            name: Ident {
                sym: sym("the_answer"),
                span: (4, 14),
            },
            params: vec![],
            return_typepath: Some(IdentPath {
                path: vec![Ident { sym: sym("UInt"), span: (18, 22) }],
                span: (18, 22),
            }),
            body: vec![Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Int(42),
                    span: (29, 31),
                }),
                span: (29, 31),
            }],
            span: (1, 33),
        }));
        assert_eq!(got, want);
    }
}
