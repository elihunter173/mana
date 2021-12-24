use codespan_reporting::{
    diagnostic::{Diagnostic, Label, Severity},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::{
    ir::{LoweringError, LoweringErrorKind},
    parse::{ParseError, ParseErrorKind},
};

type DiagFile<'a> = SimpleFile<&'a str, &'a str>;

pub fn diagnostic_from_parse_error(err: &ParseError) -> Diagnostic<()> {
    match err.kind {
        ParseErrorKind::UnexpectedEOF => Diagnostic::new(Severity::Error)
            .with_message("unexpected end of file")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)]),
        ParseErrorKind::UnexpectedToken(tok) => Diagnostic::new(Severity::Error)
            .with_message("unexpected token")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)
                .with_message(format!("unexpected token `{}`", tok.kind))]),
    }
}

pub fn diagnostic_from_lowering_error(err: &LoweringError) -> Diagnostic<()> {
    // TODO: Use Display instead of Debug. This requires threading the interner through (oh how I
    // wish for Rust contexts)
    match err.kind {
        LoweringErrorKind::UnknownType(path) => Diagnostic::new(Severity::Error)
            .with_message(format!("unknown type `{}`", path))
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)]),
        LoweringErrorKind::TypeConflict { want, got } => Diagnostic::new(Severity::Error)
            .with_message("expected type `{}`")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)
                .with_message(format!(
                    "expected type `{:?}`, got `{:?}`",
                    want, got
                ))]),
        LoweringErrorKind::InvalidType(ty) => Diagnostic::new(Severity::Error)
            .with_message(format!("invalid type `{:?}`", ty))
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)]),
    }
}

pub fn emit(file: &DiagFile<'_>, diag: &Diagnostic<()>) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config {
        chars: term::Chars::ascii(),
        ..Default::default()
    };

    term::emit(&mut writer.lock(), &config, file, diag).expect("emit diagnostic");
}
