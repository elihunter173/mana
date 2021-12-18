use crate::{
    ast::Item,
    lex::Lexer,
    parse::{ParseError, Parser},
};

#[derive(Debug, PartialEq)]
pub struct Parsed {
    pub items: Vec<Item>,
}

#[salsa::query_group(ParserGroup)]
pub trait Program: salsa::Database {
    /// The name of the source code
    #[salsa::input]
    fn filename(&self) -> String;

    /// The source code to parse
    #[salsa::input]
    fn source_code(&self) -> String;

    fn parse(&self) -> Result<Parsed, ParseError>;
}

fn parse(db: &dyn Program) -> Result<Parsed, ParseError> {
    let code = db.source_code();
    let mut parser = Parser::new(&code);
    // TODO: Improve error handling
    match parser.items() {
        Ok(items) => Ok(Parsed { items }),
        Err(err) => Err(err),
    }
}

#[salsa::database(ParserGroup)]
#[derive(Default)]
pub struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}
