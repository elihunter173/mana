use crate::{ast::Item, grammar::ProgramParser, lexer::Lexer};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parsed {
    pub items: Vec<Item>,
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
    let code = db.source_code();
    let lexer = Lexer::new(&code);
    // TODO: Improve error handling
    match parser.parse(&code, lexer) {
        Ok(items) => Ok(Parsed { items }),
        Err(err) => Err(err.to_string()),
    }
}

#[salsa::database(ParserGroup)]
#[derive(Default)]
pub struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}
