use std::{fmt, iter};

use logos::{Lexer as LogosLexer, Logos};

pub type Loc = usize;

// TODO: I should probably make this a token kind? How do I want to handle identifiers, literals,
// and other data-holding tokens?
#[derive(Logos, Copy, Clone, Debug, Eq, PartialEq)]
pub enum Tok<'input> {
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
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    // Keywords
    #[token("fn")]
    Fn,
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
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,

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
    Ident(&'input str),
    // TODO: Maybe be more liberal with allowed input?
    #[regex(r#""([^\\"]|\\")*""#)]
    String(&'input str),
    // TODO: You can't have multiple regexes for a single item
    #[regex(r"[0-9][_0-9]*")]
    #[regex(r"0x[_0-9a-fA-F]*[0-9a-fA-F][_0-9a-fA-F]*")]
    #[regex(r"0o[_0-7]*[0-7][_0-7]*")]
    #[regex(r"0b[_01]*[01][_01]*")]
    Int(&'input str),
    // TODO: You can't have multiple regexes for a single item
    // Has a decimal
    #[regex(r"[0-9][_0-9]*\.[0-9][_0-9]*")]
    // Has a scientific thing
    #[regex(r"[0-9][_0-9]*(\.[0-9][_0-9]*)?[eE][+-]?[_0-9]*[0-9][_0-9]*")]
    Float(&'input str),

    // Whitespace and other errors
    #[error]
    #[regex(r"[ \t\r\f]+", logos::skip)]
    Error,
}

impl fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Tok::Comma => ",",
            Tok::Colon => ":",
            Tok::Semicolon => ";",
            Tok::Newline => "\n",
            Tok::SingleArrow => "->",
            Tok::LParen => "(",
            Tok::RParen => ")",
            Tok::LCurly => "{",
            Tok::RCurly => "}",
            Tok::LSquare => "[",
            Tok::RSquare => "]",

            Tok::Equals => "=",
            Tok::Dot => ".",
            Tok::DotDot => "..",
            Tok::DotDotEq => "..=",
            Tok::Plus => "+",
            Tok::Minus => "-",
            Tok::Star => "*",
            Tok::Slash => "/",

            Tok::Fn => "fn",
            Tok::Let => "let",
            Tok::If => "if",
            Tok::Else => "else",
            Tok::For => "for",
            Tok::While => "while",
            Tok::Loop => "loop",
            Tok::And => "and",
            Tok::Or => "or",
            Tok::Not => "not",

            Tok::DoubleEquals => "==",
            Tok::BangEquals => "!=",
            Tok::Gt => ">",
            Tok::Geq => ">=",
            Tok::Lt => "<",
            Tok::Leq => "<=",

            Tok::True => "true",
            Tok::False => "false",
            // TODO: The String is malformatted
            Tok::Ident(s) | Tok::String(s) | Tok::Int(s) | Tok::Float(s) => s,

            Tok::Error => "ERR",
        })
    }
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
    // i can't figure out a nicer way than justl isting this ugly type
    inner: iter::Chain<
        logos::SpannedIter<'input, Tok<'input>>,
        iter::Once<(Tok<'input>, logos::Span)>,
    >,
    prev: Option<Tok<'input>>,
    offset: usize,
}

// LALRPOP lexer stream format:
// http://lalrpop.github.io/lalrpop/lexer_tutorial/002_writing_custom_lexer.html
pub type Spanned<'input> = Result<(Loc, Tok<'input>, Loc), LexError>;

impl<'input> Lexer<'input> {
    pub fn new(mut text: &'input str) -> Self {
        // Ignore the shebang if it exists
        let mut offset = 0;
        if text.starts_with("#!") {
            offset = text.find('\n').unwrap() + 1;
            text = &text[offset..];
        }

        Self {
            inner: LogosLexer::new(text)
                .spanned()
                .chain(std::iter::once((Tok::Newline, text.len()..text.len() + 1))),
            prev: None,
            offset,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<'input>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            // TODO: Is self.prev handled sensibly here? That is, should I update self.prev to be
            // Tok::Newline?
            Some((Tok::Newline, span)) => match self.prev {
                // TODO: Figure out what the rules I want are...
                // Should also look at the next token
                None
                | Some(
                    Tok::Comma
                    | Tok::Semicolon
                    | Tok::Newline
                    | Tok::LCurly
                    | Tok::LParen
                    | Tok::LSquare
                    | Tok::Plus
                    | Tok::Minus
                    | Tok::Star
                    | Tok::Slash
                    | Tok::And
                    | Tok::Or
                    | Tok::Equals,
                ) => self.next(),
                _ => {
                    self.prev = Some(Tok::Semicolon);
                    Some(Ok((
                        self.offset + span.start,
                        Tok::Semicolon,
                        self.offset + span.end,
                    )))
                }
            },

            // TODO: Handle errors

            // Normal token
            None => None,
            Some((tok, span)) => {
                self.prev = Some(tok);
                Some(Ok((self.offset + span.start, tok, self.offset + span.end)))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn lexes_as(input: &str, expected: &[(usize, Tok, usize)]) {
        let expected: Vec<_> = expected.iter().cloned().map(Ok).collect();
        let spans: Vec<_> = Lexer::new(input).collect();
        assert_eq!(expected.as_slice(), spans.as_slice());
    }

    #[test]
    fn test_punctuation() {
        lexes_as(
            ",()",
            &[
                (0, Tok::Comma, 1),
                (1, Tok::LParen, 2),
                (2, Tok::RParen, 3),
                (3, Tok::Semicolon, 4),
            ],
        );
    }

    #[test]
    fn test_semicolon_insertion() {
        lexes_as(
            "let a = 5\nprint(a + 4.5)",
            &[
                (0, Tok::Let, 3),
                (4, Tok::Ident("a"), 5),
                (6, Tok::Equals, 7),
                (8, Tok::Int("5"), 9),
                (9, Tok::Semicolon, 10),
                (10, Tok::Ident("print"), 15),
                (15, Tok::LParen, 16),
                (16, Tok::Ident("a"), 17),
                (18, Tok::Plus, 19),
                (20, Tok::Float("4.5"), 23),
                (23, Tok::RParen, 24),
                // There's no newline but it still inserts a semicolon at the end! (as if there's a
                // phantom newline)
                (24, Tok::Semicolon, 25),
            ],
        );
    }

    #[test]
    fn test_semicolons_method_chaining() {
        lexes_as(
            "foo\n.bar()\n.baz()",
            &[
                (0, Tok::Ident("foo"), 3),
                (4, Tok::Dot, 5),
                (5, Tok::Ident("bar"), 8),
                (8, Tok::LParen, 9),
                (9, Tok::RParen, 10),
                (11, Tok::Dot, 12),
                (12, Tok::Ident("baz"), 15),
                (15, Tok::LParen, 16),
                (16, Tok::RParen, 17),
                (17, Tok::Semicolon, 18),
            ],
        );
    }

    #[test]
    fn test_string() {
        lexes_as(
            "\"Hello, World!\"",
            &[
                (0, Tok::String("\"Hello, World!\""), 15),
                (15, Tok::Semicolon, 16),
            ],
        );
    }

    #[test]
    fn test_int_hex() {
        lexes_as(
            "0xDEADbeef",
            &[(0, Tok::Int("0xDEADbeef"), 10), (11, Tok::Semicolon, 12)],
        );
    }

    #[test]
    fn test_shebang() {
        lexes_as(
            "#!/usr/bin/env mana\r\n(()\r\n",
            &[
                (21, Tok::LParen, 22),
                (22, Tok::LParen, 23),
                (23, Tok::RParen, 24),
                (25, Tok::Semicolon, 26),
            ],
        );
    }
}
