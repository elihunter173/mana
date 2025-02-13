mod ast;
mod diagnostic;
mod eval;
mod intern;
mod ir;
mod resolve;
mod ty;

use crate::{ast::parse::parse_module, eval::Machine, ir::lower::lower_module};

pub fn run_code(code: &str) -> String {
    let mut symbols = intern::SymbolInterner::new();
    let (module, diagnostics) = parse_module(code, &mut symbols);
    let diagnostic_file = codespan_reporting::files::SimpleFile::new("test", code);
    if !diagnostics.is_empty() {
        for diag in &diagnostics {
            diagnostic::emit(&diagnostic_file, diag);
        }
        panic!("emitted diagnostics");
    }

    let module = match lower_module(&module, &mut symbols) {
        Ok(module) => module,
        Err(err) => {
            let diag = diagnostic::diagnostic_from_lowering_error(&err);
            diagnostic::emit(&diagnostic_file, &diag);
            panic!("emitted diagnostics");
        }
    };

    let mut vm = Machine::new(module.ir, &symbols, &module.registry);
    let val = vm.main();
    match val {
        eval::Val::Unit => "()".to_string(),
        eval::Val::Bool(v) => v.to_string(),
        eval::Val::UInt8(v) => v.to_string(),
        eval::Val::UInt16(v) => v.to_string(),
        eval::Val::UInt32(v) => v.to_string(),
        eval::Val::UInt64(v) => v.to_string(),
        eval::Val::Int8(v) => v.to_string(),
        eval::Val::Int16(v) => v.to_string(),
        eval::Val::Int32(v) => v.to_string(),
        eval::Val::Int64(v) => v.to_string(),
        eval::Val::Float32(v) => v.to_string(),
        eval::Val::Float64(v) => v.to_string(),
        eval::Val::Func(_) => todo!(),
    }
}
