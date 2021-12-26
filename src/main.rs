#![feature(map_try_insert)]
#![feature(iter_intersperse)]
// I'm working on things and those errors are noisy
#![allow(dead_code)]
// matches!() sometimes doesn't work with rust-analyzer, and I think the code is similarly
// readable, so I just ignore matches!()
#![allow(clippy::match_like_matches_macro)]

mod ast;
mod diagnostic;
mod intern;
mod ir;
mod jit;
mod lex;
mod parse;
mod ty;
// TODO: Re-enable salsa
// mod queries;

use std::fs;

use clap::{Parser, Subcommand};
// use rustyline::{error::ReadlineError, Editor};

use crate::{jit::JIT, lex::Lexer};

#[derive(Parser)]
#[clap(version = "0.1.0", author = "Eli W. Hunter <elihunter173@gmail.com>")]
struct Opts {
    #[clap(subcommand)]
    subcmd: Option<ManaCommand>,
}

#[derive(Subcommand)]
enum ManaCommand {
    Lex { path: String },
    Parse { path: String },
    Run { path: String },
}

fn main() {
    let opts = Opts::parse();

    match opts.subcmd {
        Some(ManaCommand::Lex { path }) => {
            lex(&path);
        }
        Some(ManaCommand::Parse { path }) => {
            parse_and_print(&path);
        }
        Some(ManaCommand::Run { path }) => {
            run(&path);
        }
        None => todo!("repl"), // repl()
    }
}

fn parse_and_print(path: &str) {
    let code = fs::read_to_string(path).unwrap();

    let mut symbol_interner = intern::SymbolInterner::new();

    // let mut db = DatabaseStruct::default();
    // db.set_source_code(program.clone());
    let mut parser = crate::parse::Parser::new(&code, &mut symbol_interner);
    match parser.items() {
        Ok(items) => {
            for x in items {
                println!("{:?}", x);
            }
        }
        Err(err) => crate::diagnostic::emit(
            &codespan_reporting::files::SimpleFile::new(path, &code),
            &crate::diagnostic::diagnostic_from_parse_error(&err),
        ),
    };
}

fn lex(path: &str) {
    let code = fs::read_to_string(path).unwrap();

    let lexer = Lexer::new(&code);
    for item in lexer {
        println!("{:?}", item);
    }
}

fn run(path: &str) {
    let code = fs::read_to_string(path).unwrap();

    let mut ty_interner = ty::TyInterner::with_primitives();
    let mut symbol_interner = intern::SymbolInterner::new();

    let mut parser = crate::parse::Parser::new(&code, &mut symbol_interner);

    let func = match parser.items() {
        Ok(mut items) => {
            assert_eq!(items.len(), 1);
            if let ast::Item::FnDef(func) = items.pop().unwrap() {
                func
            } else {
                panic!("only support one function right now");
            }
        }
        Err(err) => {
            crate::diagnostic::emit(
                &codespan_reporting::files::SimpleFile::new(path, &code),
                &crate::diagnostic::diagnostic_from_parse_error(&err),
            );
            return;
        }
    };

    let lowering_ctx = ir::LoweringContext {
        ty_interner: &mut ty_interner,
        symbol_interner: &symbol_interner,
    };
    let func = match lowering_ctx.lower_fn_def(&func) {
        Ok(func) => func,
        Err(err) => {
            crate::diagnostic::emit(
                &codespan_reporting::files::SimpleFile::new(path, &code),
                &crate::diagnostic::diagnostic_from_lowering_error(&err),
            );
            return;
        }
    };

    let mut jit = JIT::new(&symbol_interner);
    let code_ptr = jit.compile(&func).unwrap();

    // SAFETY: Whee! Hopefully the JIT compiler actually did compile to an arg-less and
    // return-value-less procedure
    let code_fn = unsafe { std::mem::transmute::<_, fn() -> i64>(code_ptr) };
    println!("{}", code_fn());
}

/*
fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    for linenum in 1.. {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                // TODO: Expand line
                let filename = format!("line {}", linenum);

                let mut parser = crate::parse::Parser::new(&line);
                match parser.expr() {
                    Ok(expr) => {
                        if parser.finished() {
                            println!("{:?}", expr);
                        } else {
                            println!("Error: Did not fully consume output");
                        }
                    }
                    Err(e) => crate::diagnostic::emit(
                        &codespan_reporting::files::SimpleFile::new(&filename, &line),
                        &crate::diagnostic::diagnostic_from_parse_error(&e),
                    ),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
*/
