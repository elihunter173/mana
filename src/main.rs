mod ast;
mod jit;
mod queries;
mod ty;
lalrpop_mod!(grammar);

use std::{fs::File, io::Read};

use bumpalo::Bump;
use clap::clap_app;
use lalrpop_util::lalrpop_mod;
use rustyline::{error::ReadlineError, Editor};

use crate::grammar::ProgramParser;
use crate::jit::JIT;

fn main() {
    // TODO: Switch to structopt version???
    let matches = clap_app!(myapp =>
        (version: "0.1.0")
        (author: "Eli W. Hunter <elihunter173@gmail.com>")
        (about: "Manali language frontend")
        (@subcommand parse =>
            (about: "Parse .mnl file")
            (@arg INPUT: +required "Sets the input file to use")
        )
        (@subcommand run =>
            (about: "Run .mnl file")
            (@arg INPUT: +required "Sets the input file to use")
        )
    )
    .get_matches();

    crate::queries::main();

    if let Some(ref matches) = matches.subcommand_matches("parse") {
        let path = matches.value_of("INPUT").unwrap();
        let file = File::open(path).unwrap();
        parse_and_print(file);
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
    let parser = ProgramParser::new();
    match parser.parse(&Bump::new(), &program) {
        Ok(stmts) => {
            for stmt in stmts {
                println!("{:?}", stmt);
            }
        }
        Err(err) => println!("Error: {}", err),
    };
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
    let parser = ProgramParser::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match parser.parse(&Bump::new(), &line) {
                    Ok(stmts) => {
                        for stmt in stmts {
                            println!("{:?}", stmt);
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
