#![feature(hash_set_entry)]
// I'm working on things and those errors are noisy
#![allow(dead_code)]

mod ast;
mod intern;
mod jit;
mod lexer;
mod parser;
mod queries;
mod ty;
lalrpop_mod!(grammar);

use std::{fs::File, io::Read};

use clap::clap_app;
use lalrpop_util::lalrpop_mod;
use rustyline::{error::ReadlineError, Editor};

use crate::{
    jit::JIT,
    lexer::Lexer,
    parser::{DatabaseStruct, Parser},
};

fn main() {
    // TODO: Switch to structopt version???
    let matches = clap_app!(myapp =>
        (version: "0.1.0")
        (author: "Eli W. Hunter <elihunter173@gmail.com>")
        (about: "Mana language frontend")
        (@subcommand lex =>
            (about: "Lex .mn file")
            (@arg INPUT: +required "Sets the input file to use")
        )
        (@subcommand parse =>
            (about: "Parse .mn file")
            (@arg INPUT: +required "Sets the input file to use")
        )
        (@subcommand run =>
            (about: "Run .mn file")
            (@arg INPUT: +required "Sets the input file to use")
        )
    )
    .get_matches();

    // crate::queries::main();

    if let Some(ref matches) = matches.subcommand_matches("parse") {
        let path = matches.value_of("INPUT").unwrap();
        let file = File::open(path).unwrap();
        parse_and_print(file);
    } else if let Some(ref matches) = matches.subcommand_matches("lex") {
        let path = matches.value_of("INPUT").unwrap();
        let file = File::open(path).unwrap();
        lex(file);
    } else if let Some(ref matches) = matches.subcommand_matches("run") {
        let path = matches.value_of("INPUT").unwrap();
        let file = File::open(path).unwrap();
        run(file);
    } else {
        repl();
    }
}

fn parse_and_print(mut f: File) {
    let mut program = String::new();
    f.read_to_string(&mut program).unwrap();

    let mut db = DatabaseStruct::default();
    db.set_source_code(program);
    match db.parse() {
        Ok(parsed) => {
            for expr in parsed.exprs {
                println!("{:?}", expr);
            }
        }
        Err(err) => println!("Error: {}", err),
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
    let code_fn = unsafe { std::mem::transmute::<_, fn() -> f64>(code_ptr) };
    println!("{}", code_fn());
}

fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut db = DatabaseStruct::default();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                db.set_source_code(line);
                match db.parse() {
                    Ok(parsed) => {
                        for expr in parsed.exprs {
                            println!("{:?}", expr);
                        }
                        // for stmt in stmts {
                        //     let mut comp = Compiler::new();
                        //     stmt.compile(&mut comp);
                        //     let mut interpreter = Interpreter::new(&comp.code, comp.immediates);
                        //     interpreter.run();
                        // }
                    }
                    Err(err) => println!("Error: {}", err),
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
