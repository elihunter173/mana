#![feature(map_try_insert)]
// I'm working on things and those errors are noisy
#![allow(dead_code)]
// matches!() sometimes doesn't work with rust-analyzer, and I think the code is similarly
// readable, so I just ignore matches!()
#![allow(clippy::match_like_matches_macro)]

mod ast;
mod diagnostic;
mod jit;
mod lex;
mod parse;
// mod queries;
mod ty;

use std::{fs::File, io::Read};

use clap::{Parser, Subcommand};
use rustyline::{error::ReadlineError, Editor};

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
            let file = File::open(path).unwrap();
            lex(file);
        }
        Some(ManaCommand::Parse { path }) => {
            let file = File::open(&path).unwrap();
            parse_and_print(&path, file);
        }
        Some(ManaCommand::Run { path }) => {
            let file = File::open(path).unwrap();
            run(file);
        }
        None => repl(),
    }
}

fn parse_and_print(path: &str, mut f: File) {
    let mut code = String::new();
    f.read_to_string(&mut code).unwrap();

    // let mut db = DatabaseStruct::default();
    // db.set_source_code(program.clone());
    let mut parser = crate::parse::Parser::new(&code);
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

fn lex(mut f: File) {
    let mut program = String::new();
    f.read_to_string(&mut program).unwrap();

    let lexer = Lexer::new(&program);
    for item in lexer {
        println!("{:?}", item);
    }
}

fn run(mut f: File) {
    let mut code = String::new();
    f.read_to_string(&mut code).unwrap();
    let mut jit = JIT::new();
    let code_ptr = jit.compile(&code).unwrap();
    // SAFETY: Whee! Hopefully the JIT compiler actually did compile to an arg-less and
    // return-value-less procedure
    let code_fn = unsafe { std::mem::transmute::<_, fn() -> i64>(code_ptr) };
    println!("{}", code_fn());
}

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
                    // TODO: Do diagnostic
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
