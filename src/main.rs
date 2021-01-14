use bumpalo::Bump;
use lalrpop_util::lalrpop_mod;
use rustyline::{error::ReadlineError, Cmd, Editor, KeyEvent, Modifiers};

mod ast;
mod bytecode;
mod compile;
mod types;
lalrpop_mod!(pub grammar);

use crate::{
    bytecode::Interpreter,
    compile::{Compile, Compiler},
    grammar::StmtsParser,
};

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    rl.bind_sequence(KeyEvent::new('c', Modifiers::SHIFT), Cmd::Newline);

    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let parser = StmtsParser::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match parser.parse(&Bump::new(), &line) {
                    Ok(stmts) => {
                        for stmt in stmts {
                            let mut comp = Compiler::new();
                            stmt.compile(&mut comp);
                            let mut interpreter = Interpreter::new(&comp.code, comp.immediates);
                            interpreter.run();
                        }
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
