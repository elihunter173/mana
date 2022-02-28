//! TODO: Remove this whole file

use codespan_reporting::{
    diagnostic,
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

use crate::ir::lower::{LoweringError, LoweringErrorKind};

pub type Diagnostic = diagnostic::Diagnostic<()>;
pub use codespan_reporting::diagnostic::{Label, Severity};

type DiagFile<'a> = SimpleFile<&'a str, &'a str>;

pub fn diagnostic_from_lowering_error(err: &LoweringError) -> Diagnostic {
    // TODO: Use Display instead of Debug. This requires threading the interner through
    match &err.kind {
        LoweringErrorKind::UnknownType(path) => Diagnostic::new(Severity::Error)
            .with_message("unknown type")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)
                .with_message(format!("unknown type `{}`", path))]),
        LoweringErrorKind::UnknownVariable(ident) => Diagnostic::new(Severity::Error)
            .with_message("unknown variable")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)
                .with_message(format!("unknown variable `{:?}`", ident))]),
        LoweringErrorKind::TypeConflict { want, got } => Diagnostic::new(Severity::Error)
            .with_message("mismatched types")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)
                .with_message(format!(
                    "expected type `{:?}`, got `{:?}`",
                    want, got
                ))]),
        LoweringErrorKind::InvalidType(ty) => Diagnostic::new(Severity::Error)
            .with_message("invalid type")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)
                .with_message(format!("type `{:?}` cannot be used here", ty))]),
        LoweringErrorKind::DuplicateItem => Diagnostic::new(Severity::Error)
            .with_message("duplicate item")
            .with_labels(vec![Label::primary((), err.span.0..err.span.1)
                .with_message("TODO: Improve error message and really whole error")]),
    }
}

pub fn emit(file: &DiagFile<'_>, diag: &Diagnostic) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config {
        chars: term::Chars::ascii(),
        ..Default::default()
    };

    term::emit(&mut writer.lock(), &config, file, diag).expect("emit diagnostic");
}
