use std::{collections::HashMap, slice};

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

use crate::{
    ast::{self, BinOp, Expr, Literal},
    parse::Parser,
    queries::{DatabaseStruct, Program},
};

// TODO: We just assume you return an int
const ASSUMED_TYPE: types::Type = cranelift::codegen::ir::types::I32;

// TODO: This needs to be done earlier
fn resolve_typepath(typepath: &ast::TypePath) -> types::Type {
    match typepath.path[0].0.as_str() {
        "Int" => cranelift::codegen::ir::types::I32,
        "F64" => cranelift::codegen::ir::types::F64,
        _ => unimplemented!(),
    }
}

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift separates this
    /// from `Module` to allow for parallel compilation, with a context per thread, though this
    /// isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the jit backend, which manages the JIT'd functions.
    module: JITModule,
}

impl JIT {
    pub fn new() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }

    // TODO: Make this accept an AST probably
    pub fn compile(&mut self, input: &str) -> anyhow::Result<*const u8> {
        // First, parse the string, producing AST nodes.
        // let (name, params, the_return, stmts) =
        //     parser::function(&input).map_err(|e| e.to_string())?;
        // TODO: This is definitely not how I should be doing this
        let mut parser = Parser::new(input);
        let func = parser.fn_def().unwrap();

        let name = "TODO";

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(&func)?;

        // Next, declare the function to jit. Functions must be declared before they can be called,
        // or defined.
        //
        // TODO: This may be an area where the API should be streamlined; should we have a version
        // of `declare_function` that automatically declares the function?
        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        // Define the function to jit. This finishes compilation, although there may be outstanding
        // relocations to perform. Currently, jit cannot finish relocations until all functions to
        // be called are defined. For this toy demo for now, we'll just finalize the function
        // below.
        self.module
            .define_function(
                id,
                &mut self.ctx,
                &mut codegen::binemit::NullTrapSink {},
                &mut codegen::binemit::NullStackMapSink {},
            )
            .unwrap();

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any outstanding relocations
        // (patching in addresses, now that they're available).
        self.module.finalize_definitions();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    /// Create a zero-initialized data section.
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        // The steps here are analogous to `compile`, except that data is much simpler than
        // functions.
        self.data_ctx.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.data_ctx)
            .map_err(|e| e.to_string())?;
        self.data_ctx.clear();
        self.module.finalize_definitions();
        let buffer = self.module.get_finalized_data(id);
        // TODO: Can we move the unsafe into cranelift?
        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    }

    // Translate from toy-language AST nodes into Cranelift IR.
    fn translate(&mut self, func: &ast::FnDef) -> anyhow::Result<()> {
        assert_eq!(func.name.0, "main");
        assert_eq!(func.params.len(), 0);

        for (name, typepath) in &func.params {
            let typ = resolve_typepath(typepath);
            self.ctx.func.signature.params.push(AbiParam::new(typ));
        }

        // Our toy language currently only supports one return value, though Cranelift is designed
        // to support more.
        if let Some(return_typepath) = &func.return_typepath {
            let typ = resolve_typepath(&return_typepath);
            self.ctx.func.signature.returns.push(AbiParam::new(typ));
        }

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to the function's
        // parameters.
        //
        // TODO: Streamline the API here.
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further predecessors. Since it's the
        // entry block, it won't have any predecessors.
        builder.seal_block(entry_block);

        // TODO: This is fundamentally the wrong approach for mana
        // The toy language allows variables to be declared implicitly. Walk the AST and declare
        // all implicitly-declared variables.
        let variables = declare_variables(&mut builder, &func.body, entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            builder,
            variables,
            module: &mut self.module,
        };
        let return_value = trans.translate_block(&func.body);

        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}

/// A collection of state used for translating from toy-language AST nodes into Cranelift IR.
struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    /// When you write out instructions in Cranelift, you get back `Value`s. You can then use these
    /// references in other instructions.
    fn translate_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Literal(Literal::Float(imm)) => self.builder.ins().f64const(*imm),
            Expr::Literal(Literal::Int(imm)) => self.builder.ins().iconst(ASSUMED_TYPE, *imm as i64),

            Expr::Binary(op, lhs, rhs) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                let b = self.builder.ins();
                // TODO: Use better types. Need more than just AST
                match op {
                    BinOp::Add => b.iadd(lhs, rhs),
                    BinOp::Sub => b.isub(lhs, rhs),
                    BinOp::Mul => b.imul(lhs, rhs),
                    BinOp::Div => b.sdiv(lhs, rhs),
                    BinOp::Eq => b.icmp(IntCC::Equal, lhs, rhs),
                    BinOp::Neq => b.icmp(IntCC::NotEqual, lhs, rhs),
                    _ => unimplemented!(),
                }
            }

            // Expr::Call(name, args) => self.translate_call(name, args),
            // Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            Expr::Ident(ident) => {
                // `use_var` is used to read the value of a variable.
                let variable = self.variables.get(&ident.0).expect("variable not defined");
                self.builder.use_var(*variable)
            }

            Expr::Let(ident, _, expr) | Expr::Set(ident, expr) => {
                // `def_var` is used to write the value of a variable. Note that variables can have
                // multiple definitions. Cranelift will convert them into SSA form for itself
                // automatically.
                let new_value = self.translate_expr(expr);
                let variable = self.variables.get(&ident.0).unwrap();
                self.builder.def_var(*variable, new_value);
                new_value
            }

            Expr::If { cond, then_expr, else_expr } => {
                self.translate_if_else(cond, then_expr, else_expr.as_deref())
            }

            Expr::Block(exprs) => self.translate_block(&exprs),

            // Expr::WhileLoop(condition, loop_body) => {
            //     self.translate_while_loop(*condition, loop_body)
            // }
            _ => unimplemented!(),
        }
    }

    fn translate_if_else(
        &mut self,
        condition: &Expr,
        then_expr: &Expr,
        else_expr: Option<&Expr>,
    ) -> Value {
        let condition_value = self.translate_expr(condition);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // If-else constructs in the toy language have a return value.
        // In traditional SSA form, this would produce a PHI between
        // the then and else bodies. Cranelift uses block parameters,
        // so set up a parameter in the merge block, and we'll pass
        // the return values to it from the branches.
        // TODO: We just assume that you return an int
        self.builder.append_block_param(merge_block, ASSUMED_TYPE);

        // Test the if condition and conditionally branch.
        if else_expr.is_some() {
            self.builder.ins().brz(condition_value, else_block, &[]);
        } else {
            self.builder.ins().brz(condition_value, merge_block, &[]);
        }
        // Fall through to then block.
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_return = self.translate_expr(then_expr);

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        if let Some(else_expr) = else_expr {
            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);
            let else_return = self.translate_expr(else_expr);

            // Jump to the merge block, passing it the block return value.
            self.builder.ins().jump(merge_block, &[else_return]);
        }

        // Switch to the merge block for subsequent statements.
        self.builder.switch_to_block(merge_block);

        // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);

        // Read the value of the if-else by reading the merge block
        // parameter.
        let phi = self.builder.block_params(merge_block)[0];
        phi
    }

    fn translate_block(&mut self, exprs: &[Expr]) -> Value {
        // Create a block which will have this value calculated
        let next_block = self.builder.create_block();
        self.builder.append_block_param(next_block, ASSUMED_TYPE);

        // Translate all the expressions and keep the last one as the return value
        let mut rtn = self.builder.ins().iconst(ASSUMED_TYPE, 0);
        for expr in exprs {
            rtn = self.translate_expr(expr);
        }

        // Switch to the next block passing in the return
        self.builder.ins().jump(next_block, &[rtn]);
        self.builder.switch_to_block(next_block);
        self.builder.seal_block(next_block);

        // Return the value of this block
        let phi = self.builder.block_params(next_block)[0];
        phi
    }

    // fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) -> Value {
    //     let header_block = self.builder.create_block();
    //     let body_block = self.builder.create_block();
    //     let exit_block = self.builder.create_block();

    //     self.builder.ins().jump(header_block, &[]);
    //     self.builder.switch_to_block(header_block);

    //     let condition_value = self.translate_expr(condition);
    //     self.builder.ins().brz(condition_value, exit_block, &[]);
    //     self.builder.ins().jump(body_block, &[]);

    //     self.builder.switch_to_block(body_block);
    //     self.builder.seal_block(body_block);

    //     for expr in loop_body {
    //         self.translate_expr(expr);
    //     }
    //     self.builder.ins().jump(header_block, &[]);

    //     self.builder.switch_to_block(exit_block);

    //     // We've reached the bottom of the loop, so there will be no
    //     // more backedges to the header to exits to the bottom.
    //     self.builder.seal_block(header_block);
    //     self.builder.seal_block(exit_block);

    //     // Just return 0 for now.
    //     self.builder.ins().iconst(self.typ, 0)
    // }

    // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
    //     let mut sig = self.module.make_signature();

    //     // Add a parameter for each argument.
    //     for _arg in &args {
    //         sig.params.push(AbiParam::new(self.typ));
    //     }

    //     // For simplicity for now, just make all calls return a single I64.
    //     sig.returns.push(AbiParam::new(self.typ));

    //     // TODO: Streamline the API here?
    //     let callee = self
    //         .module
    //         .declare_function(&name, Linkage::Import, &sig)
    //         .expect("problem declaring function");
    //     let local_callee = self
    //         .module
    //         .declare_func_in_func(callee, &mut self.builder.func);

    //     let mut arg_values = Vec::new();
    //     for arg in args {
    //         arg_values.push(self.translate_expr(arg))
    //     }
    //     let call = self.builder.ins().call(local_callee, &arg_values);
    //     self.builder.inst_results(call)[0]
    // }

    // fn translate_global_data_addr(&mut self, name: String) -> Value {
    //     let sym = self
    //         .module
    //         .declare_data(&name, Linkage::Export, true, false)
    //         .expect("problem declaring data object");
    //     let local_id = self
    //         .module
    //         .declare_data_in_func(sym, &mut self.builder.func);

    //     let pointer = self.module.target_config().pointer_type();
    //     self.builder.ins().symbol_value(pointer, local_id)
    // }
}

fn declare_variables(
    builder: &mut FunctionBuilder,
    exprs: &[Expr],
    _entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for expr in exprs {
        declare_variables_in_expr(builder, &mut variables, &mut index, expr);
    }

    variables
}

/// Recursively descend through the AST, translating all implicit variable declarations.
fn declare_variables_in_expr(
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    expr: &Expr,
) {
    if let Expr::Let(ident, typepath, _expr) = expr {
        let var = Variable::new(*index);
        if !variables.contains_key(&ident.0) {
            variables.insert(ident.0.to_owned(), var);
            let typ =
                resolve_typepath(typepath.as_ref().expect("type inference not supported yet"));
            builder.declare_var(var, typ);
            *index += 1;
        }
    }

    expr.apply_children(|e| declare_variables_in_expr(builder, variables, index, e));
}
