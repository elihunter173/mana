#![feature(map_try_insert)]
#![feature(hash_set_entry)]
#![feature(iter_intersperse)]
// matches!() sometimes doesn't work with rust-analyzer, and I think the code is similarly
// readable, so I just ignore matches!()
#![allow(clippy::match_like_matches_macro)]

mod ast;
mod diagnostic;
mod intern;
mod ir;
mod jit;
mod ty;
// TODO: Re-enable salsa
// mod queries;

use std::fs;

use clap::{Parser, Subcommand};
// use rustyline::{error::ReadlineError, Editor};

use crate::{
    ast::{lex::Lexer, parse::Parser as ManaParser},
    ir::lower::Lowerer,
    jit::JIT,
};

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
    DumpIr { path: String },
    Run { path: String },
}

fn main() {
    let opts = Opts::parse();

    match opts.subcmd {
        Some(ManaCommand::Lex { path }) => {
            lex(&path);
        }
        Some(ManaCommand::Parse { path }) => {
            dump_ast(&path);
        }
        Some(ManaCommand::DumpIr { path }) => {
            dump_ir(&path);
        }
        Some(ManaCommand::Run { path }) => {
            run(&path);
        }
        None => todo!("repl"), // repl()
    }
}

fn lex(path: &str) {
    let code = fs::read_to_string(path).unwrap();

    let lexer = Lexer::new(&code);
    for item in lexer {
        println!("{:?}", item);
    }
}

fn dump_ast(path: &str) {
    let code = fs::read_to_string(path).unwrap();

    let mut symbol_interner = intern::SymbolInterner::new();
    let mut parser = ManaParser::new(&code, &mut symbol_interner);
    let module = match parser.module() {
        Ok(module) => module,
        Err(err) => {
            crate::diagnostic::emit(
                &codespan_reporting::files::SimpleFile::new(path, &code),
                &crate::diagnostic::diagnostic_from_parse_error(&err),
            );
            return;
        }
    };
    println!("{:?}", module);
}

fn dump_ir(path: &str) {
    let code = fs::read_to_string(path).unwrap();

    let mut symbol_interner = intern::SymbolInterner::new();
    let mut parser = ManaParser::new(&code, &mut symbol_interner);
    let module = match parser.module() {
        Ok(module) => module,
        Err(err) => {
            crate::diagnostic::emit(
                &codespan_reporting::files::SimpleFile::new(path, &code),
                &crate::diagnostic::diagnostic_from_parse_error(&err),
            );
            return;
        }
    };

    let mut registry = ir::registry::Registry::with_basic_types();
    let mut resolver = ir::resolve::Resolver::with_prelude(&mut symbol_interner, &mut registry);
    let mut lowerer = Lowerer {
        resolver: &mut resolver,
        symbol_interner: &mut symbol_interner,
        registry: &mut registry,
    };
    let module = match lowerer.lower_module(&module) {
        Ok(module) => module,
        Err(err) => {
            crate::diagnostic::emit(
                &codespan_reporting::files::SimpleFile::new(path, &code),
                &crate::diagnostic::diagnostic_from_lowering_error(&err),
            );
            return;
        }
    };

    println!("{:?}", registry);
    println!("{:?}", module);
}

fn run(path: &str) {
    let code = fs::read_to_string(path).unwrap();

    let mut symbol_interner = intern::SymbolInterner::new();
    let mut parser = ManaParser::new(&code, &mut symbol_interner);
    let module = match parser.module() {
        Ok(module) => module,
        Err(err) => {
            crate::diagnostic::emit(
                &codespan_reporting::files::SimpleFile::new(path, &code),
                &crate::diagnostic::diagnostic_from_parse_error(&err),
            );
            return;
        }
    };

    let mut registry = ir::registry::Registry::with_basic_types();
    let mut resolver = ir::resolve::Resolver::with_prelude(&mut symbol_interner, &mut registry);
    let mut lowerer = Lowerer {
        resolver: &mut resolver,
        symbol_interner: &mut symbol_interner,
        registry: &mut registry,
    };
    let module = match lowerer.lower_module(&module) {
        Ok(module) => module,
        Err(err) => {
            crate::diagnostic::emit(
                &codespan_reporting::files::SimpleFile::new(path, &code),
                &crate::diagnostic::diagnostic_from_lowering_error(&err),
            );
            return;
        }
    };

    let mut jit = JIT::new(&symbol_interner, &registry);
    let code_ptr = jit.compile(&module).unwrap();

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
