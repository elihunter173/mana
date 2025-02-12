// matches!() sometimes doesn't work with rust-analyzer, and I think the code is similarly
// readable, so I just ignore matches!()
#![allow(clippy::match_like_matches_macro)]

mod ast;
mod diagnostic;
mod eval;
mod intern;
mod ir;
mod resolve;
mod ty;

use std::fs;

use clap::{Parser, Subcommand};
use eval::Machine;
// use rustyline::{error::ReadlineError, Editor};

use crate::{
    ast::{lex::Lexer, parse::parse_module},
    ir::lower::lower_module,
};

#[derive(Parser)]
#[clap(version = "0.1.0", author = "Eli W. Hunter <elihunter173@gmail.com>")]
struct Opts {
    #[clap(subcommand)]
    subcmd: Option<ManaCommand>,
}

#[derive(Subcommand)]
enum ManaCommand {
    Run(RunOpts),
}

#[derive(Parser)]
struct RunOpts {
    path: String,
    #[clap(long)]
    dump_tokens: bool,
    #[clap(long)]
    dump_ast: bool,
    #[clap(long)]
    dump_ir: bool,
}

fn main() {
    let opts = Opts::parse();

    match opts.subcmd {
        Some(ManaCommand::Run(opts)) => {
            run(&opts);
        }
        None => todo!("repl"), // repl()
    }
}

fn run(opts: &RunOpts) {
    let code = fs::read_to_string(&opts.path).unwrap();

    if opts.dump_tokens {
        let lexer = Lexer::new(&code);
        for item in lexer {
            println!("{:?}", item);
        }
    }

    println!("Building {}...", opts.path);
    let mut symbols = intern::SymbolInterner::new();
    let (module, diagnostics) = parse_module(&code, &mut symbols);
    let diagnostic_file =
        codespan_reporting::files::SimpleFile::new(opts.path.as_str(), code.as_str());
    if opts.dump_ast {
        println!("AST: {}", module.display(&symbols));
    }
    if !diagnostics.is_empty() {
        for diag in &diagnostics {
            diagnostic::emit(&diagnostic_file, diag);
        }
        return;
    }

    let module = match lower_module(&module, &mut symbols) {
        Ok(module) => module,
        Err(err) => {
            let diag = diagnostic::diagnostic_from_lowering_error(&err);
            diagnostic::emit(&diagnostic_file, &diag);
            return;
        }
    };
    if opts.dump_ir {
        println!("{:?}", module.registry);
        println!("{:?}", module.ir);
    }

    let mut vm = Machine::new(module.ir, &symbols, &module.registry);
    vm.start();

    // let mut jit = JIT::new(&symbols, &module.registry);
    // let code_ptr = jit.compile(&module.ir);
    //
    // // SAFETY: Whee! Hopefully the JIT compiler actually did compile to an arg-less and
    // // return-value-less procedure
    // let code_fn = unsafe { std::mem::transmute::<_, fn() -> i32>(code_ptr) };
    // println!("Build finished. Running.");
    // println!("{}", code_fn());
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
