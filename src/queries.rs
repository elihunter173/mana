use crate::{ast::Item, lex::Lexer, parse::Parser};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parsed {
    pub items: Vec<Item>,
}

#[salsa::query_group(ParserGroup)]
pub trait Program: salsa::Database {
    /// The source code to parse
    #[salsa::input]
    fn source_code(&self) -> String;

    fn parse(&self) -> Result<Parsed, String>;
}

fn parse(db: &dyn Program) -> Result<Parsed, String> {
    let code = db.source_code();
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer);
    // TODO: Improve error handling
    match parser.items() {
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
