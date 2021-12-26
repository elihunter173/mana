use crate::{
    ast::Item,
    lex::Lexer,
    parse::{ParseError, Parser},
};

// TODO: Look at https://github.com/Kixiron/codespan-salsa/blob/master/src/main.rs

#[derive(Debug, PartialEq)]
pub struct Parsed {
    pub items: Vec<Item>,
}

#[salsa::query_group(ProgramGroup)]
pub trait Program: salsa::Database {
    // TODO: How do we pipe the source code through well?
    // TODO: Probably want to use Arc

    /// The source code to parse
    #[salsa::input]
    fn source_code(&self, filename: Path) -> String;

    fn parse(&self, filename: Path) -> Result<Parsed, ParseError>;
}

fn parse(db: &dyn Program, filename: Path) -> Result<Parsed, ParseError> {
    let code = db.source_code(filename);
    let mut parser = Parser::new(&code);
    // TODO: Improve error handling
    match parser.items() {
        Ok(items) => Ok(Parsed { items }),
        Err(err) => Err(err),
    }
}

#[salsa::database(ProgramGroup)]
#[derive(Default)]
pub struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}
