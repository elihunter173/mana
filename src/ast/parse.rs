use crate::{
    ast::{
        lex::{Lexer, Token, TokenKind},
        *,
    },
    diagnostic::{Diagnostic, Label},
    intern::SymbolInterner,
};

struct Parser<'input> {
    code: &'input str,
    // TODO: I need some way to avoid cascading errors. Not sure a good way to do that tho...
    // erroring: bool,
    diagnostics: Vec<Diagnostic>,
    lexer: Lexer<'input>,
    symbols: &'input mut SymbolInterner,
}

pub fn parse_module(code: &str, symbols: &mut SymbolInterner) -> (Module, Vec<Diagnostic>) {
    let mut parser = Parser::new(code, symbols);
    let module = parser.module();
    (module, parser.diagnostics)
}

impl<'input> Parser<'input> {
    fn new(code: &'input str, symbols: &'input mut SymbolInterner) -> Self {
        Self {
            code,
            diagnostics: Vec::new(),
            lexer: Lexer::new(code),
            symbols,
        }
    }
}

// Helpers
impl<'input> Parser<'input> {
    // Returns `Token` of the given `kind` if it matches the given type.
    #[must_use]
    fn maybe_token(&mut self, kind: TokenKind) -> Option<Token> {
        let tok = self.lexer.peek()?;
        if tok.kind != kind {
            return None;
        }
        self.lexer.next();
        Some(tok)
    }

    fn maybe_ident(&mut self) -> Option<Ident> {
        let tok = self.maybe_token(TokenKind::Ident)?;
        Some(Ident {
            sym: self.symbols.get_or_intern(self.lexer.source(&tok)),
            span: tok.span,
        })
    }

    fn maybe_ident_path(&mut self) -> Option<IdentPath> {
        let head = self.maybe_ident()?;
        let mut span = head.span;
        let mut path = vec![head];
        while let Some(_dot) = self.maybe_token(TokenKind::Dot) {
            let next = self.maybe_ident()?;
            span.1 = next.span.1;
            path.push(next);
        }
        Some(IdentPath { path, span })
    }

    // TODO: Remove these and have better diagnostics
    fn unexpected_token(&self, tok: Token) -> Diagnostic {
        Diagnostic::error()
            .with_message("unexpected token")
            .with_labels(vec![Label::primary((), tok.span.0..tok.span.1)])
    }

    fn unexpected_eof(&self) -> Diagnostic {
        Diagnostic::error()
            .with_message("unexpected end of file")
            .with_labels(vec![Label::primary(
                (),
                self.code.len() - 1..self.code.len(),
            )])
    }

    fn unexpected(&mut self) {
        if let Some(tok) = self.lexer.peek() {
            self.diagnostics.push(self.unexpected_token(tok))
        } else {
            self.diagnostics.push(self.unexpected_eof())
        }
    }
}

impl<'input> Parser<'input> {
    fn module(&mut self) -> Module {
        // TODO: Figure out a way to re-use delimited
        let mut items = Vec::new();
        if self.lexer.peek().is_some() {
            items.push(self.item());
        }
        while self.maybe_token(TokenKind::Semicolon).is_some() {
            if self.lexer.peek().is_none() {
                break;
            }
            items.push(self.item());
        }

        if let Some(tok) = self.lexer.next() {
            self.diagnostics.push(self.unexpected_token(tok));
        }

        Module { items }
    }

    fn item(&mut self) -> Item {
        let tok = if let Some(tok) = self.lexer.peek() {
            tok
        } else {
            self.diagnostics.push(self.unexpected_eof());
            return Item::Error;
        };

        match tok.kind {
            TokenKind::Fn => self.fn_def(),
            TokenKind::Import => self.import(),
            _ => {
                // TODO: How do I want to handle these errors? Maybe try to parse a whole
                // expression?

                let message = if tok.kind == TokenKind::Let {
                    "unexpected variable declaration".to_owned()
                } else {
                    format!("unexpected token {}", tok.kind)
                };
                // Try to prevent cascading errors
                self.diagnostics.push(
                    Diagnostic::error()
                        .with_message(message)
                        .with_labels(vec![Label::primary((), tok.span.0..tok.span.1)
                            .with_message("expected one of `fn` or `import`")]),
                );
                // Seek to semicolon
                loop {
                    match self.lexer.peek() {
                        None => break,
                        Some(tok) if tok.kind == TokenKind::Semicolon => break,
                        _ => self.lexer.next(),
                    };
                }
                Item::Error
            }
        }
    }

    fn fn_def(&mut self) -> Item {
        let fn_ = self
            .maybe_token(TokenKind::Fn)
            .expect("functions parsed predictively");

        let name = if let Some(ident) = self.maybe_ident() {
            ident
        } else {
            self.unexpected();
            return Item::Error;
        };

        if self.maybe_token(TokenKind::LParen).is_none() {
            self.unexpected();
            return Item::Error;
        }

        // TODO: Try to make delimited work again
        let maybe_param = |parser: &mut Self| {
            let ident = parser.maybe_ident()?;
            parser.maybe_token(TokenKind::Colon)?;
            let typepath = parser.maybe_ident_path()?;
            Some((ident, typepath))
        };
        let mut params = Vec::new();
        if let Some(tok) = self.maybe_token(TokenKind::RParen) {
            tok
        } else {
            // Initial element
            if let Some(param) = maybe_param(self) {
                params.push(param);
            } else {
                self.unexpected();
                return Item::Error;
            }

            // Remaining elements
            loop {
                let got_comma = self.maybe_token(TokenKind::Comma).is_some();
                if let Some(tok) = self.maybe_token(TokenKind::RParen) {
                    // We finished
                    break tok;
                } else if !got_comma {
                    // We didn't get a comma, so we expect to finish, but didn't
                    self.unexpected();
                    return Item::Error;
                } else {
                    // Expect another parameters
                    if let Some(param) = maybe_param(self) {
                        params.push(param);
                    } else {
                        self.unexpected();
                        return Item::Error;
                    }
                }
            }
        };

        let return_type = if self.maybe_token(TokenKind::Colon).is_some() {
            if let Some(ident_path) = self.maybe_ident_path() {
                Some(ident_path)
            } else {
                self.unexpected();
                return Item::Error;
            }
        } else {
            None
        };

        let (body, (_start, end)) = self.block();
        Item::FnDef(FnDef {
            name,
            params,
            return_typepath: return_type,
            body,
            span: (fn_.span.0, end),
        })
    }

    fn import(&mut self) -> Item {
        self.maybe_token(TokenKind::Import)
            .expect("import is parsed predictively");

        let path = if let Some(path) = self.maybe_ident_path() {
            path
        } else {
            self.unexpected();
            return Item::Error;
        };

        if self.maybe_token(TokenKind::Semicolon).is_none() {
            self.unexpected();
            return Item::Error;
        }

        Item::Import(path)
    }

    fn expr(&mut self) -> Expr {
        self.expr_at_bp(0)
    }

    // TODO: I probably need ControlFlow again to prevent infinite loops with ExprKind::Errors. Or
    // maybe just short circuit on ExprKind::Error

    // This is a precedence climbing parser
    // Built with the help of https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    // Binding powers stolen from https://wren.io/syntax.html
    fn expr_at_bp(&mut self, min_bp: u8) -> Expr {
        let tok = if let Some(tok) = self.lexer.peek() {
            tok
        } else {
            self.diagnostics.push(self.unexpected_eof());
            return Expr {
                kind: ExprKind::Error,
                span: (self.code.len() - 1, self.code.len()),
            };
        };

        let mut lhs = match tok.kind {
            TokenKind::Minus => self.parse_unary(UnaryOp::Neg, 8),
            TokenKind::Not => self.parse_unary(UnaryOp::Lnot, 8),
            TokenKind::Tilde => self.parse_unary(UnaryOp::Bnot, 8),

            // TODO: I hate duplicating this. Try to merge literal in here
            TokenKind::Int
            | TokenKind::IntHex
            | TokenKind::IntOct
            | TokenKind::IntBin
            | TokenKind::Float
            | TokenKind::String
            | TokenKind::True
            | TokenKind::False => self.literal(),

            TokenKind::Ident => {
                let ident = self.maybe_ident().expect("peeked");
                Expr {
                    span: ident.span,
                    kind: ExprKind::Ident(ident),
                }
            }

            TokenKind::LParen => {
                self.lexer.next();
                let lparen = tok;
                let expr = self.expr_at_bp(0);
                if let Some(rparen) = self.maybe_token(TokenKind::RParen) {
                    Expr {
                        span: (lparen.span.0, rparen.span.1),
                        kind: expr.kind,
                    }
                } else {
                    self.unexpected();
                    return Expr {
                        span: (tok.span.0, expr.span.1),
                        kind: ExprKind::Error,
                    };
                }
            }

            TokenKind::LCurly if min_bp == 0 => {
                let (block, span) = self.block();
                Expr { kind: ExprKind::Block(block), span }
            }
            TokenKind::Loop if min_bp == 0 => self.loop_loop(),
            TokenKind::While if min_bp == 0 => self.while_loop(),
            TokenKind::Break if min_bp == 0 => self.break_(),
            TokenKind::Continue if min_bp == 0 => self.continue_(),
            TokenKind::Return if min_bp == 0 => self.return_(),

            TokenKind::If if min_bp == 0 => self.if_chain(),
            TokenKind::Let if min_bp == 0 => self.let_(),

            _ => {
                self.unexpected();
                Expr {
                    kind: ExprKind::Error,
                    span: tok.span,
                }
            }
        };

        loop {
            let tok = if let Some(tok) = self.lexer.peek() {
                tok
            } else {
                break;
            };

            fn binop(
                parser: &mut Parser,
                lhs: Expr,
                op: BinOp,
                binding_power: u8,
                assoc: Assoc,
            ) -> Expr {
                parser.lexer.next();
                let bp = match assoc {
                    Assoc::Left => binding_power + 1,
                    Assoc::Right => binding_power,
                };
                let rhs = parser.expr_at_bp(bp);
                Expr {
                    span: (lhs.span.0, rhs.span.1),
                    kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                }
            }

            // Force the left hand side to be an ident
            // TODO: This is bad, but making changes to this requires significant changes to
            // everything else
            fn conv_ident(expr: Expr) -> Ident {
                if let ExprKind::Ident(ident) = expr.kind {
                    ident
                } else {
                    panic!("must be ident");
                }
            }

            // Handle trailing operators
            lhs = match tok.kind {
                TokenKind::Equals if min_bp <= 1 => self.set(lhs, None),
                TokenKind::PlusEq if min_bp <= 1 => self.set(lhs, Some(BinOp::Add)),
                TokenKind::MinusEq if min_bp <= 1 => self.set(lhs, Some(BinOp::Sub)),
                TokenKind::StarEq if min_bp <= 1 => self.set(lhs, Some(BinOp::Mul)),
                TokenKind::SlashEq if min_bp <= 1 => self.set(lhs, Some(BinOp::Div)),

                TokenKind::Or if min_bp <= 2 => binop(self, lhs, BinOp::Lor, 2, Assoc::Left),

                TokenKind::And if min_bp <= 3 => binop(self, lhs, BinOp::Land, 3, Assoc::Left),

                TokenKind::DoubleEquals if min_bp <= 4 => {
                    binop(self, lhs, BinOp::Eq, 4, Assoc::Left)
                }
                TokenKind::BangEquals if min_bp <= 4 => {
                    binop(self, lhs, BinOp::Neq, 4, Assoc::Left)
                }

                TokenKind::Lt if min_bp <= 5 => binop(self, lhs, BinOp::Lt, 5, Assoc::Left),
                TokenKind::Leq if min_bp <= 5 => binop(self, lhs, BinOp::Leq, 5, Assoc::Left),
                TokenKind::Gt if min_bp <= 5 => binop(self, lhs, BinOp::Gt, 5, Assoc::Left),
                TokenKind::Geq if min_bp <= 5 => binop(self, lhs, BinOp::Geq, 5, Assoc::Left),

                // TODO: Bitwise operators in between comparison and arithmetic
                TokenKind::Plus if min_bp <= 6 => binop(self, lhs, BinOp::Add, 6, Assoc::Left),
                TokenKind::Minus if min_bp <= 6 => binop(self, lhs, BinOp::Sub, 6, Assoc::Left),

                TokenKind::Star if min_bp <= 7 => binop(self, lhs, BinOp::Mul, 7, Assoc::Left),
                TokenKind::Slash if min_bp <= 7 => binop(self, lhs, BinOp::Div, 7, Assoc::Left),
                TokenKind::Percent if min_bp <= 7 => binop(self, lhs, BinOp::Rem, 7, Assoc::Left),

                TokenKind::LParen if min_bp <= 9 => {
                    let fn_ident = conv_ident(lhs);
                    let (args, args_span) = self.fn_args();
                    Expr {
                        kind: ExprKind::FnCall(fn_ident, args),
                        span: (fn_ident.span.0, args_span.1),
                    }
                }
                TokenKind::LSquare if min_bp <= 9 => {
                    self.lexer.next();
                    let expr = self.expr_at_bp(0);
                    if let Some(rsquare) = self.maybe_token(TokenKind::RSquare) {
                        Expr {
                            span: (lhs.span.0, rsquare.span.1),
                            kind: ExprKind::Index(Box::new(lhs), Box::new(expr)),
                        }
                    } else {
                        self.unexpected();
                        return Expr {
                            span: (lhs.span.0, expr.span.1),
                            kind: ExprKind::Error,
                        };
                    }
                }
                TokenKind::Dot if min_bp <= 9 => {
                    self.lexer.next();
                    let dot = tok;
                    if let Some(ident) = self.maybe_ident() {
                        Expr {
                            span: (lhs.span.0, ident.span.1),
                            kind: ExprKind::Access(Box::new(lhs), ident),
                        }
                    } else {
                        self.unexpected();
                        return Expr {
                            span: (lhs.span.0, dot.span.1),
                            kind: ExprKind::Error,
                        };
                    }
                }

                _ => break,
            };
        }

        lhs
    }

    // TODO: This is a terrible hack I need to get rid of
    fn delimited_expr_hack_todo_replace(
        &mut self,
        start: TokenKind,
        delimiter: TokenKind,
        end: TokenKind,
    ) -> (Vec<Expr>, Span) {
        let exprs = Vec::new();

        let start_tok = if let Some(tok) = self.maybe_token(start) {
            tok
        } else {
            self.unexpected();
            // TODO: I need to actually have the span
            return (exprs, (0, 0));
        };

        let mut exprs = Vec::new();
        let end_tok = if let Some(tok) = self.maybe_token(end) {
            tok
        } else {
            // Initial element
            exprs.push(self.expr());

            // Remaining elements
            loop {
                let got_comma = self.maybe_token(delimiter).is_some();
                if let Some(tok) = self.maybe_token(end) {
                    // We finished
                    break tok;
                } else if !got_comma {
                    // We didn't get a comma, so we expect to finish, but didn't
                    self.unexpected();
                    // TODO: Actually get the span
                    return (exprs, (0, 0));
                } else {
                    // Expect another parameters
                    exprs.push(self.expr());
                }
            }
        };

        (exprs, (start_tok.span.0, end_tok.span.1))
    }

    fn block(&mut self) -> (Vec<Expr>, Span) {
        self.delimited_expr_hack_todo_replace(
            TokenKind::LCurly,
            TokenKind::Semicolon,
            TokenKind::RCurly,
        )
    }

    fn loop_loop(&mut self) -> Expr {
        let loop_ = self
            .maybe_token(TokenKind::Loop)
            .expect("loop loop is parsed predictively");

        let (exprs, block_span) = self.block();
        Expr {
            span: (loop_.span.0, block_span.1),
            kind: ExprKind::Loop(Box::new(Expr {
                span: block_span,
                kind: ExprKind::Block(exprs),
            })),
        }
    }

    fn while_loop(&mut self) -> Expr {
        let while_ = self
            .maybe_token(TokenKind::While)
            .expect("while loop is parsed predictively");

        let cond_expr = self.expr();
        let sugar_span = cond_expr.span;

        // TODO: Have block build into vec so we don't copy all these exprs
        let if_break = Expr {
            span: sugar_span,
            kind: ExprKind::If {
                cond: Box::new(Expr {
                    span: sugar_span,
                    kind: ExprKind::Unary(UnaryOp::Lnot, Box::new(cond_expr)),
                }),
                then_expr: Box::new(Expr {
                    span: sugar_span,
                    kind: ExprKind::Break(None),
                }),
                else_expr: None,
            },
        };
        let (mut exprs, block_span) = self.block();
        exprs.insert(0, if_break);

        Expr {
            span: (while_.span.0, block_span.1),
            kind: ExprKind::Loop(Box::new(Expr {
                span: block_span,
                kind: ExprKind::Block(exprs),
            })),
        }
    }

    fn keyword_expr(
        &mut self,
        keyword_kind: TokenKind,
        kind_builder: impl FnOnce(Option<Box<Expr>>) -> ExprKind,
    ) -> Expr {
        let keyword = self
            .maybe_token(keyword_kind)
            .expect(&format!("{keyword_kind} parsed predictively"));
        let expr = if let Some(Token { kind: TokenKind::Semicolon, .. }) = self.lexer.peek() {
            None
        } else {
            Some(self.expr())
        };
        let span = (
            keyword.span.0,
            expr.as_ref()
                .map(|expr| expr.span.1)
                .unwrap_or(keyword.span.1),
        );

        Expr {
            kind: kind_builder(expr.map(Box::new)),
            span,
        }
    }

    fn break_(&mut self) -> Expr {
        self.keyword_expr(TokenKind::Break, ExprKind::Break)
    }

    fn continue_(&mut self) -> Expr {
        self.keyword_expr(TokenKind::Continue, ExprKind::Continue)
    }

    fn return_(&mut self) -> Expr {
        self.keyword_expr(TokenKind::Return, ExprKind::Return)
    }

    fn if_chain(&mut self) -> Expr {
        let if_ = self
            .maybe_token(TokenKind::If)
            .expect("if chain is parsed predictively");

        let cond = self.expr();
        let (then_block, true_block_span) = self.block();

        let false_expr = if self.maybe_token(TokenKind::Else).is_some() {
            let tok = if let Some(tok) = self.lexer.peek() {
                tok
            } else {
                self.diagnostics.push(self.unexpected_eof());
                return Expr {
                    kind: ExprKind::Error,
                    span: (self.code.len() - 1, self.code.len()),
                };
            };
            if tok.kind == TokenKind::If {
                Some(self.if_chain())
            } else {
                let (false_block, false_block_span) = self.block();
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
        Expr {
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_expr: Box::new(Expr {
                    kind: ExprKind::Block(then_block),
                    span: true_block_span,
                }),
                else_expr: false_expr.map(Box::new),
            },
            span,
        }
    }

    fn fn_args(&mut self) -> (Vec<Expr>, Span) {
        self.delimited_expr_hack_todo_replace(
            TokenKind::LParen,
            TokenKind::Comma,
            TokenKind::RParen,
        )
    }

    fn let_(&mut self) -> Expr {
        let let_ = self
            .maybe_token(TokenKind::Let)
            .expect("let is parsed predictively");

        let ident = if let Some(ident) = self.maybe_ident() {
            ident
        } else {
            self.unexpected();
            return Expr {
                kind: ExprKind::Error,
                span: let_.span,
            };
        };
        let typepath = if let Some(colon) = self.maybe_token(TokenKind::Colon) {
            if let Some(ident_path) = self.maybe_ident_path() {
                Some(ident_path)
            } else {
                self.unexpected();
                return Expr {
                    kind: ExprKind::Error,
                    span: (let_.span.0, colon.span.1),
                };
            }
        } else {
            None
        };

        if self.maybe_token(TokenKind::Equals).is_none() {
            self.unexpected();
            return Expr {
                kind: ExprKind::Error,
                span: (
                    let_.span.0,
                    typepath
                        .map(|typepath| typepath.span.1)
                        .unwrap_or(ident.span.1),
                ),
            };
        }

        let expr = self.expr();

        let span = (let_.span.0, expr.span.1);
        Expr {
            kind: ExprKind::Let(ident, typepath, Box::new(expr)),
            span,
        }
    }

    fn set(&mut self, lhs: Expr, op: Option<BinOp>) -> Expr {
        let ident = if let ExprKind::Ident(ident) = lhs.kind {
            ident
        } else {
            self.diagnostics.push(
                Diagnostic::error()
                    .with_message("invalid assignment expression")
                    .with_labels(vec![Label::primary((), lhs.span.0..lhs.span.1)
                        .with_message("must assign to identifier")]),
            );
            return Expr {
                kind: ExprKind::Error,
                span: lhs.span,
            };
        };

        // Sitting on assignment symbol
        self.lexer.next();

        let expr = self.expr();

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
        Expr {
            kind: ExprKind::Set(ident, Box::new(assigned_expr)),
            span,
        }
    }

    fn parse_unary(&mut self, op: UnaryOp, binding_power: u8) -> Expr {
        // This is a prefix
        let tok = self.lexer.next().expect("peeked token already");
        let expr = self.expr_at_bp(binding_power);
        Expr {
            span: (tok.span.0, expr.span.1),
            kind: ExprKind::Unary(op, Box::new(expr)),
        }
    }

    fn literal(&mut self) -> Expr {
        let tok = self.lexer.next().expect("peeked already");
        let tok_src = self.lexer.source(&tok);
        let kind = match tok.kind {
            TokenKind::Int => LiteralKind::Int(parse_int(tok_src, 10)),
            TokenKind::IntHex => LiteralKind::Int(parse_int(&tok_src[2..], 16)),
            TokenKind::IntOct => LiteralKind::Int(parse_int(&tok_src[2..], 8)),
            TokenKind::IntBin => LiteralKind::Int(parse_int(&tok_src[2..], 2)),
            TokenKind::Float => {
                LiteralKind::Float(self.symbols.get_or_intern(&parse_float(tok_src)))
            }
            TokenKind::String => {
                LiteralKind::String(self.symbols.get_or_intern(&parse_string(tok_src)))
            }
            TokenKind::True => LiteralKind::Bool(true),
            TokenKind::False => LiteralKind::Bool(false),
            _ => unreachable!("peeked already"),
        };

        Expr {
            kind: ExprKind::Literal(kind),
            span: tok.span,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Assoc {
    Left,
    Right,
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

fn parse_float(input: &str) -> String {
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

    #[track_caller]
    fn expect_op(op: BinOp, expr: &Expr) -> (&Expr, &Expr) {
        match &expr.kind {
            ExprKind::Binary(actual_op, left, right) if *actual_op == op => {
                (left.as_ref(), right.as_ref())
            }
            _ => panic!("expected {:?}, expr: {:?}", op, expr),
        }
    }

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
        let (got, mut sym) = run_parser("Foo.Bar", |p| p.maybe_ident_path());

        let want = Some(IdentPath {
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
        let (expr, _) = run_parser("1 + 2 == 3 and true", |p| p.expr());
        assert_eq!(expr.span, (0, 19));
        let (left, _right) = expect_op(BinOp::Land, &expr);
        assert_eq!(left.span, (0, 10));
        let (left, _right) = expect_op(BinOp::Eq, &left);
        assert_eq!(left.span, (0, 5));
        expect_op(BinOp::Add, &left);
    }

    #[test]
    #[ignore = "I'm not sure this is what I want"]
    fn logical_same_level() {
        let (got, _) = run_parser("true and false or true", |p| p.expr());
        assert_eq!(got.kind, ExprKind::Error);
    }

    #[test]
    fn arithmetic_precedance() {
        let (expr, _) = run_parser("1 + 2 * 3 / (4 - 5)", |p| p.expr());
        assert_eq!(expr.span, (0, 19));
        let (_left, right) = expect_op(BinOp::Add, &expr);
        assert_eq!(right.span, (4, 19));
        let (left, right) = expect_op(BinOp::Div, &right);
        expect_op(BinOp::Mul, left);
        assert_eq!(right.span, (12, 19));
        expect_op(BinOp::Sub, &right);
    }

    #[test]
    fn comparison_precedance() {
        let (expr, _) = run_parser("1 + 2 == 3", |p| p.expr());
        assert_eq!(expr.span, (0, 10));
        let (left, _right) = expect_op(BinOp::Eq, &expr);
        assert_eq!(left.span, (0, 5));
        expect_op(BinOp::Add, &left);
    }

    #[test]
    fn fn_calls() {
        let (got, mut sym) = run_parser("foo(n, \"bar\") + 1", |p| p.expr());

        let want = Expr {
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
                                kind: ExprKind::Literal(LiteralKind::String(sym("bar"))),
                            },
                        ],
                    ),
                }),
                Box::new(Expr {
                    span: (16, 17),
                    kind: ExprKind::Literal(LiteralKind::Int(1)),
                }),
            ),
        };
        assert_eq!(got, want);
    }

    #[test]
    fn test_fn_def() {
        let code = "
fn the_answer(): UInt {
    42
}";
        let (got, mut sym) = run_parser(code, |p| p.item());

        let want = Item::FnDef(FnDef {
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
                kind: ExprKind::Literal(LiteralKind::Int(42)),
                span: (29, 31),
            }],
            span: (1, 33),
        });
        assert_eq!(got, want);
    }
}
