use codespan_reporting::{
    diagnostic::{Diagnostic, Label, Severity},
    files::{Files, SimpleFile},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::parse::{ParseError, ParseErrorKind};

type DiagFile<'a> = SimpleFile<&'a str, &'a str>;

pub fn diagnostic_from_parse_error<'a>(err: &ParseError) -> Diagnostic<()> {
    match err.kind {
        ParseErrorKind::EndOfStream => {
            Diagnostic::new(Severity::Error).with_message("unexpected end of file")
        }
        ParseErrorKind::UnexpectedToken(tok) => Diagnostic::new(Severity::Error)
            .with_message("unexpected token")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)
                .with_message(format!("unexpected token `{}`", tok.kind))]),
    }
}

pub fn emit<'a>(file: &DiagFile<'a>, diag: &Diagnostic<()>) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let mut config = term::Config::default();
    config.chars = term::Chars::ascii();

    term::emit(&mut writer.lock(), &config, file, &diag).expect("emit diagnostic");
}
