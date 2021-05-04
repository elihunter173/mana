use crate::ast::Expr;
use crate::grammar::ProgramParser;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parsed {
    pub exprs: Vec<Expr>,
}

#[salsa::query_group(ParserGroup)]
pub trait Parser: salsa::Database {
    /// The source code to parse
    #[salsa::input]
    fn source_code(&self) -> String;

    fn parse(&self) -> Result<Parsed, String>;
}

fn parse(db: &dyn Parser) -> Result<Parsed, String> {
    let parser = ProgramParser::new();
    // TODO: Improve error handling
    match parser.parse(&db.source_code()) {
        Ok(exprs) => Ok(Parsed { exprs }),
        Err(err) => Err(err.to_string()),
    }
}

#[salsa::database(ParserGroup)]
#[derive(Default)]
pub struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}
