use codespan_reporting::{
    diagnostic::{Diagnostic, Label, Severity},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::parse::{ParseError, ParseErrorKind};

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

pub fn emit(file: &DiagFile<'_>, diag: &Diagnostic<()>) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config {
        chars: term::Chars::ascii(),
        ..Default::default()
    };

    term::emit(&mut writer.lock(), &config, file, diag).expect("emit diagnostic");
}
