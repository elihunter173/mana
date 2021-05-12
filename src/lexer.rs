use std::str::CharIndices;

type Loc = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum Tok {
    // Punctuation
    Colon,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,

    // Operators
    /// `=`
    Equals,
    /// `.`
    Dot,
    /// `..`
    DotDot,
    /// `..=`
    DotDotEq,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,

    // Keywords
    Fn,
    Let,
    If,
    Else,
    For,
    While,
    And,
    Or,
    Not,
    True,
    False,

    // Comparisons
    /// `==`
    DoubleEquals,
    /// `!=`
    BangEquals,
    /// `>`
    Gt,
    /// `>=`
    Geq,
    /// `<`
    Lt,
    /// `<=`
    Leq,
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexError {
    // Not possible
}

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
}

// LALRPOP lexer stream format:
// http://lalrpop.github.io/lalrpop/lexer_tutorial/002_writing_custom_lexer.html
pub type Spanned = Result<(Loc, Tok, Loc), LexError>;

impl<'input> Lexer<'input> {
    pub fn new(text: &'input str) -> Self {
        Self {
            chars: text.char_indices(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Spanned;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.next() {
                // TODO: Holy fuck the boilerplate
                Some((i, ',')) => return Some(Ok((i, Tok::Comma, i + 1))),
                Some((i, ':')) => return Some(Ok((i, Tok::Colon, i + 1))),
                Some((i, ';')) => return Some(Ok((i, Tok::Semicolon, i + 1))),
                Some((i, '(')) => return Some(Ok((i, Tok::LParen, i + 1))),
                Some((i, ')')) => return Some(Ok((i, Tok::RParen, i + 1))),
                Some((i, '{')) => return Some(Ok((i, Tok::LCurly, i + 1))),
                Some((i, '}')) => return Some(Ok((i, Tok::RCurly, i + 1))),
                Some((i, '[')) => return Some(Ok((i, Tok::LSquare, i + 1))),
                Some((i, ']')) => return Some(Ok((i, Tok::RSquare, i + 1))),

                Some((i, '+')) => return Some(Ok((i, Tok::Plus, i + 1))),
                Some((i, '-')) => return Some(Ok((i, Tok::Minus, i + 1))),
                Some((i, '*')) => return Some(Ok((i, Tok::Star, i + 1))),
                Some((i, '/')) => return Some(Ok((i, Tok::Slash, i + 1))),

                // End of file
                None => return None,
                // Comment
                _ => continue,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_punctuation() {
        let test = ",()";
        let spans: Vec<_> = Lexer::new(test).collect();
        assert_eq!(3, spans.len());
        assert_eq!(Ok((0, Tok::Comma, 1)), spans[0]);
        assert_eq!(Ok((1, Tok::LParen, 2)), spans[1]);
        assert_eq!(Ok((2, Tok::RParen, 3)), spans[2]);
    }
}
