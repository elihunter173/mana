use bumpalo::Bump;

use crate::ast::Expr;
use crate::grammar::ProgramParser;

#[derive(Debug, PartialEq, Eq)]
struct ParseResult<'expr> {
    exprs: Vec<Expr<'expr>>,
}

#[salsa::query_group(ParserGroup)]
trait Parser: salsa::Database {
    /// The source code to parse
    #[salsa::input]
    fn source_code(&self) -> String;

    fn parse(&self) -> ParseResult<'static>;
}

fn parse(db: &dyn Parser) -> ParseResult {
    let parser = ProgramParser::new();
    ParseResult {
        exprs: parser.parse(&Bump::new(), &db.source_code()).unwrap(),
    }
}

#[salsa::database(ParserGroup)]
#[derive(Default)]
pub struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}
