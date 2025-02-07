use std::fmt;

use logos::{Lexer as LogosLexer, Logos};

// TODO: Maybe I should have an EOF token

// TODO: Maybe I should have the lexer keep track of newlines not converted to semicolons (i.e.
// newlines) and expose that information as a `.raw_next()` and `.raw_peek()` for better
// diagnostics in the parser. Same thing with not skipping comments?

#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    // Punctuation
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    // We have the LogosLexer keep track of newlines, so that we can convert significant newlines
    // to semicolons based off the automatic semicolon-insertion rule. The reason we do this is
    // because we need to know what the previous and next tokens are for the semicolon-insertion.
    #[token("\n")]
    Newline,
    #[token("->")]
    SingleArrow,
    #[token("@")]
    At,
    #[token("\\")]
    Backslash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,

    // Operators
    #[token("=")]
    Equals,
    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token("..=")]
    DotDotEq,
    #[token("+")]
    Plus,
    #[token("+=")]
    PlusEq,
    #[token("-")]
    Minus,
    #[token("-=")]
    MinusEq,
    #[token("*")]
    Star,
    #[token("*=")]
    StarEq,
    #[token("/")]
    Slash,
    #[token("/=")]
    SlashEq,
    #[token("%")]
    Percent,
    #[token("%=")]
    PercentEq,
    #[token("&")]
    Ampersand,
    #[token("&=")]
    AmpersandEq,
    #[token("|")]
    Bar,
    #[token("|=")]
    BarEq,
    #[token("^")]
    Caret,
    #[token("^=")]
    CaretEq,
    #[token("<<")]
    LtLt,
    #[token(">>")]
    GtGt,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    #[token("~")]
    Tilde,

    // Keywords
    #[token("def")]
    Def,
    #[token("fn")]
    Fn,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("type")]
    Type,
    #[token("impl")]
    Impl,
    #[token("import")]
    Import,
    #[token("pub")]
    Pub,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("loop")]
    Loop,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,

    // Comparisons
    #[token("==")]
    DoubleEquals,
    #[token("!=")]
    BangEquals,
    #[token(">")]
    Gt,
    #[token(">=")]
    Geq,
    #[token("<")]
    Lt,
    #[token("<=")]
    Leq,

    // Atoms
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"[a-zA-Z_][0-9a-zA-Z_]*")]
    Ident,
    #[regex(r"\$[0-9]+")]
    ClosureArg,
    #[regex(r#""([^\\"]|\\")*""#)]
    String,
    #[regex(r"[0-9][_0-9]*")]
    Int,
    #[regex(r"0x_*[0-9a-fA-F][_0-9a-fA-F]*")]
    IntHex,
    #[regex(r"0o_*[0-7][_0-7]*")]
    IntOct,
    #[regex(r"0b_*[01][_01]*")]
    IntBin,
    // First option has mandatory decimal and no e suffix. Section option has optional decimal and
    // an e suffix
    #[regex(r"[0-9][_0-9]*(\.[0-9][_0-9]*)?[eE][+-]?_*[0-9][_0-9]*")]
    #[regex(r"[0-9][_0-9]*\.[0-9][_0-9]*")]
    Float,

    #[regex(r"//.*\n", logos::skip)]
    Comment,
    #[error]
    // Whitespace
    #[regex(r"[ \t\r\f]+", logos::skip)]
    Error,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Semicolon => ";",
            TokenKind::Newline => "\\n",
            TokenKind::SingleArrow => "->",
            TokenKind::At => "@",
            TokenKind::Backslash => "\\",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LCurly => "{",
            TokenKind::RCurly => "}",
            TokenKind::LSquare => "[",
            TokenKind::RSquare => "]",

            TokenKind::Equals => "=",
            TokenKind::Dot => ".",
            TokenKind::DotDot => "..",
            TokenKind::DotDotEq => "..=",
            TokenKind::Plus => "+",
            TokenKind::PlusEq => "+=",
            TokenKind::Minus => "-",
            TokenKind::MinusEq => "-=",
            TokenKind::Star => "*",
            TokenKind::StarEq => "*=",
            TokenKind::Slash => "/",
            TokenKind::SlashEq => "/=",
            TokenKind::Percent => "%",
            TokenKind::PercentEq => "%=",
            TokenKind::Ampersand => "&",
            TokenKind::AmpersandEq => "&=",
            TokenKind::Bar => "|",
            TokenKind::BarEq => "|=",
            TokenKind::Caret => "^",
            TokenKind::CaretEq => "^=",
            TokenKind::LtLt => "<<",
            TokenKind::GtGt => ">>",
            TokenKind::And => "and",
            TokenKind::Or => "or",
            TokenKind::Not => "not",
            TokenKind::Tilde => "~",

            TokenKind::Def => "def",
            TokenKind::Fn => "fn",
            TokenKind::Struct => "struct",
            TokenKind::Enum => "enum",
            TokenKind::Trait => "trait",
            TokenKind::Type => "type",
            TokenKind::Impl => "impl",
            TokenKind::Import => "import",
            TokenKind::Pub => "pub",
            TokenKind::Let => "let",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::For => "for",
            TokenKind::While => "while",
            TokenKind::Loop => "loop",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Return => "return",

            TokenKind::DoubleEquals => "==",
            TokenKind::BangEquals => "!=",
            TokenKind::Gt => ">",
            TokenKind::Geq => ">=",
            TokenKind::Lt => "<",
            TokenKind::Leq => "<=",

            TokenKind::True => "true",
            TokenKind::False => "false",

            TokenKind::Ident => "identifier",
            TokenKind::ClosureArg => "closure parameter",
            TokenKind::String => "string",
            TokenKind::Int
            | TokenKind::IntHex
            | TokenKind::IntOct
            | TokenKind::IntBin
            | TokenKind::Float => "number",

            TokenKind::Comment => "comment",

            TokenKind::Error => "error",
        })
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: (usize, usize),
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexError {
    // Not possible
}

impl fmt::Display for LexError {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

pub struct Lexer<'input> {
    inner: LogosLexer<'input, TokenKind>,
    peeked: Option<Option<Token>>,
    previous: Option<Token>,
    next: Option<Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(text: &'input str) -> Self {
        let mut lexer = LogosLexer::new(text);

        // Ignore shebang
        if text.starts_with("#!") {
            let newline = text.find('\n').unwrap_or(text.len());
            lexer.bump(newline);
        }

        Self {
            inner: lexer,
            peeked: None,
            previous: None,
            next: None,
        }
    }

    pub fn source(&self, token: &Token) -> &'input str {
        &self.inner.source()[token.span.0..token.span.1]
    }

    pub fn peek(&mut self) -> Option<Token> {
        if let Some(peeked) = self.peeked {
            peeked
        } else {
            let next = self.next();
            self.peeked = Some(next);
            next
        }
    }

    fn raw_next_token(&mut self) -> Option<Token> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.inner.next().map(|kind| {
                let span = self.inner.span();
                Token { kind, span: (span.start, span.end) }
            }),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let current = match self.peeked.take() {
            Some(v) => v,
            None => self.next.take().or_else(|| self.raw_next_token()),
        };

        let rtn = match current {
            Some(Token { kind: TokenKind::Newline, span }) => {
                let next = loop {
                    match self.raw_next_token() {
                        Some(Token { kind: TokenKind::Newline, .. }) => {}
                        next => break next,
                    }
                };

                if should_skip_semicolon(self.previous.map(|t| t.kind), next.map(|t| t.kind)) {
                    next
                } else {
                    self.next = next;
                    Some(Token { kind: TokenKind::Semicolon, span })
                }
            }

            // Normal token
            Some(lexeme) => Some(lexeme),
            None => None,
        };

        self.previous = rtn;
        rtn
    }
}

fn should_skip_semicolon(prev: Option<TokenKind>, next: Option<TokenKind>) -> bool {
    let yes_bc_prev = match prev {
        None
        | Some(
            TokenKind::Comma
            | TokenKind::Semicolon
            | TokenKind::Newline
            | TokenKind::LCurly
            | TokenKind::LParen
            | TokenKind::LSquare
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Equals,
        ) => true,
        _ => false,
    };
    let yes_bc_next = match next {
        Some(TokenKind::Dot) => true,
        _ => false,
    };
    yes_bc_prev || yes_bc_next
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_lex(input: &str, expected: &[(usize, TokenKind, usize)]) {
        let expected: Vec<_> = expected
            .iter()
            .map(|&(start, kind, end)| Token { kind, span: (start, end) })
            .collect();
        let actual: Vec<_> = Lexer::new(input).collect();
        assert_eq!(expected.as_slice(), actual.as_slice());
    }

    #[test]
    fn test_punctuation() {
        assert_lex(
            ",()",
            &[
                (0, TokenKind::Comma, 1),
                (1, TokenKind::LParen, 2),
                (2, TokenKind::RParen, 3),
            ],
        );
    }

    #[test]
    fn test_semicolon_insertion() {
        assert_lex(
            "let a = 5\n\n\nprint(a + 4.5)",
            &[
                (0, TokenKind::Let, 3),
                (4, TokenKind::Ident, 5),
                (6, TokenKind::Equals, 7),
                (8, TokenKind::Int, 9),
                (9, TokenKind::Semicolon, 10),
                (12, TokenKind::Ident, 17),
                (17, TokenKind::LParen, 18),
                (18, TokenKind::Ident, 19),
                (20, TokenKind::Plus, 21),
                (22, TokenKind::Float, 25),
                (25, TokenKind::RParen, 26),
            ],
        );
    }

    #[test]
    fn test_semicolons_method_chaining() {
        assert_lex(
            "foo\n.bar()\n\n.baz()\n//Finish it off\n.broke()",
            &[
                (0, TokenKind::Ident, 3),
                (4, TokenKind::Dot, 5),
                (5, TokenKind::Ident, 8),
                (8, TokenKind::LParen, 9),
                (9, TokenKind::RParen, 10),
                (12, TokenKind::Dot, 13),
                (13, TokenKind::Ident, 16),
                (16, TokenKind::LParen, 17),
                (17, TokenKind::RParen, 18),
                (35, TokenKind::Dot, 36),
                (36, TokenKind::Ident, 41),
                (41, TokenKind::LParen, 42),
                (42, TokenKind::RParen, 43),
            ],
        );
    }

    #[test]
    fn test_ident() {
        assert_lex("foo", &[(0, TokenKind::Ident, 3)]);
    }

    #[test]
    fn test_closure_arg() {
        assert_lex("$123", &[(0, TokenKind::ClosureArg, 4)]);
    }

    #[test]
    fn test_lambda() {
        assert_lex(
            "\\a, b -> (a*a) + (2*a*b) + (b*b)",
            &[
                (0, TokenKind::Backslash, 1),
                (1, TokenKind::Ident, 2),
                (2, TokenKind::Comma, 3),
                (4, TokenKind::Ident, 5),
                (6, TokenKind::SingleArrow, 8),
                //
                (9, TokenKind::LParen, 10),
                (10, TokenKind::Ident, 11),
                (11, TokenKind::Star, 12),
                (12, TokenKind::Ident, 13),
                (13, TokenKind::RParen, 14),
                //
                (15, TokenKind::Plus, 16),
                //
                (17, TokenKind::LParen, 18),
                (18, TokenKind::Int, 19),
                (19, TokenKind::Star, 20),
                (20, TokenKind::Ident, 21),
                (21, TokenKind::Star, 22),
                (22, TokenKind::Ident, 23),
                (23, TokenKind::RParen, 24),
                //
                (25, TokenKind::Plus, 26),
                //
                (27, TokenKind::LParen, 28),
                (28, TokenKind::Ident, 29),
                (29, TokenKind::Star, 30),
                (30, TokenKind::Ident, 31),
                (31, TokenKind::RParen, 32),
            ],
        );
    }

    #[test]
    fn test_string() {
        assert_lex("\"Hello, World!\"", &[(0, TokenKind::String, 15)]);
    }

    #[test]
    fn test_int_hex() {
        assert_lex("0xDEADbeef", &[(0, TokenKind::IntHex, 10)]);
    }

    #[test]
    fn test_int_oct() {
        assert_lex("0o755", &[(0, TokenKind::IntOct, 5)]);
    }

    #[test]
    fn test_int_bin() {
        assert_lex("0b1111_0011", &[(0, TokenKind::IntBin, 11)]);
    }

    #[test]
    fn test_float() {
        assert_lex("1_000.123_456e+3", &[(0, TokenKind::Float, 16)]);
    }

    #[test]
    fn test_fn_def() {
        assert_lex(
            r#"
// This is just a silly function to test comments
fn foo() -> UInt {
    print("running foo")
    42
}
"#,
            &[
                (51, TokenKind::Fn, 53),
                (54, TokenKind::Ident, 57),
                (57, TokenKind::LParen, 58),
                (58, TokenKind::RParen, 59),
                (60, TokenKind::SingleArrow, 62),
                (63, TokenKind::Ident, 67),
                (68, TokenKind::LCurly, 69),
                (74, TokenKind::Ident, 79),
                (79, TokenKind::LParen, 80),
                (80, TokenKind::String, 93),
                (93, TokenKind::RParen, 94),
                (94, TokenKind::Semicolon, 95),
                (99, TokenKind::Int, 101),
                (101, TokenKind::Semicolon, 102),
                (102, TokenKind::RCurly, 103),
                (103, TokenKind::Semicolon, 104),
            ],
        );
    }

    #[test]
    fn test_shebang() {
        assert_lex(
            "#!/usr/bin/env mana\r\n(()\r\n",
            &[
                (21, TokenKind::LParen, 22),
                (22, TokenKind::LParen, 23),
                (23, TokenKind::RParen, 24),
                (25, TokenKind::Semicolon, 26),
            ],
        );
    }
}
