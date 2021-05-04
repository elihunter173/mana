use std::collections::HashMap;
use std::slice;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

use crate::{
    ast::{BinOp, Expr, Literal},
    grammar::ProgramParser,
};

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

    /// Compile a string in the toy language into machine code.
    pub fn compile(&mut self, input: &str) -> anyhow::Result<*const u8> {
        // First, parse the string, producing AST nodes.
        // let (name, params, the_return, stmts) =
        //     parser::function(&input).map_err(|e| e.to_string())?;
        let program = ProgramParser::new().parse(&input).unwrap();

        let name = "TODO";

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(&program)?;

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
            .define_function(id, &mut self.ctx, &mut codegen::binemit::NullTrapSink {})
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
    fn translate(&mut self, exprs: &[Expr]) -> anyhow::Result<()> {
        // Our toy language currently only supports I64 values, though Cranelift supports other
        // types.
        let typ = cranelift::codegen::ir::types::F64;

        // TODO: Currently we don't return anything

        let the_return = "ret";

        // for _p in &params {
        //     self.ctx.func.signature.params.push(AbiParam::new(typ));
        // }

        // Our toy language currently only supports one return value, though Cranelift is designed
        // to support more.
        self.ctx.func.signature.returns.push(AbiParam::new(typ));

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

        // The toy language allows variables to be declared implicitly. Walk the AST and declare
        // all implicitly-declared variables.
        let variables = declare_variables(typ, &mut builder, exprs, entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            typ,
            builder,
            variables,
            module: &mut self.module,
        };
        for expr in exprs {
            trans.translate_expr(expr);
        }

        // Set up the return variable of the function. Above, we declared a variable to hold the
        // return value. Here, we just do a use of that variable.
        let return_variable = trans.variables.get(the_return).unwrap();
        let return_value = trans.builder.use_var(*return_variable);

        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}

/// A collection of state used for translating from toy-language AST nodes into Cranelift IR.
struct FunctionTranslator<'a> {
    typ: types::Type,
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
            Expr::Literal(_) => unimplemented!(),

            Expr::Binary(op, lhs, rhs) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                let b = self.builder.ins();
                match op {
                    BinOp::Add => b.fadd(lhs, rhs),
                    BinOp::Sub => b.fsub(lhs, rhs),
                    BinOp::Mul => b.fmul(lhs, rhs),
                    BinOp::Div => b.fdiv(lhs, rhs),
                    BinOp::Eq => b.fcmp(FloatCC::Equal, lhs, rhs),
                    BinOp::Neq => b.fcmp(FloatCC::NotEqual, lhs, rhs),
                    _ => unimplemented!(),
                }
            }

            // Expr::Call(name, args) => self.translate_call(name, args),
            // Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            Expr::Ident(ident) => {
                // `use_var` is used to read the value of a variable.
                let variable = self
                    .variables
                    .get(&ident.name)
                    .expect("variable not defined");
                self.builder.use_var(*variable)
            }

            Expr::Let(ident, expr) | Expr::Set(ident, expr) => {
                // `def_var` is used to write the value of a variable. Note that variables can have
                // multiple definitions. Cranelift will convert them into SSA form for itself
                // automatically.
                let new_value = self.translate_expr(expr);
                let variable = self.variables.get(&ident.name).unwrap();
                self.builder.def_var(*variable, new_value);
                new_value
            }

            // Expr::IfElse(condition, then_body, else_body) => {
            //     self.translate_if_else(*condition, then_body, else_body)
            // }
            // Expr::WhileLoop(condition, loop_body) => {
            //     self.translate_while_loop(*condition, loop_body)
            // }
            _ => unimplemented!(),
        }
    }

    // fn translate_if_else(
    //     &mut self,
    //     condition: Expr,
    //     then_body: Vec<Expr>,
    //     else_body: Vec<Expr>,
    // ) -> Value {
    //     let condition_value = self.translate_expr(condition);

    //     let then_block = self.builder.create_block();
    //     let else_block = self.builder.create_block();
    //     let merge_block = self.builder.create_block();

    //     // If-else constructs in the toy language have a return value.
    //     // In traditional SSA form, this would produce a PHI between
    //     // the then and else bodies. Cranelift uses block parameters,
    //     // so set up a parameter in the merge block, and we'll pass
    //     // the return values to it from the branches.
    //     self.builder.append_block_param(merge_block, self.typ);

    //     // Test the if condition and conditionally branch.
    //     self.builder.ins().brz(condition_value, else_block, &[]);
    //     // Fall through to then block.
    //     self.builder.ins().jump(then_block, &[]);

    //     self.builder.switch_to_block(then_block);
    //     self.builder.seal_block(then_block);
    //     let mut then_return = self.builder.ins().iconst(self.typ, 0);
    //     for expr in then_body {
    //         then_return = self.translate_expr(expr);
    //     }

    //     // Jump to the merge block, passing it the block return value.
    //     self.builder.ins().jump(merge_block, &[then_return]);

    //     self.builder.switch_to_block(else_block);
    //     self.builder.seal_block(else_block);
    //     let mut else_return = self.builder.ins().iconst(self.typ, 0);
    //     for expr in else_body {
    //         else_return = self.translate_expr(expr);
    //     }

    //     // Jump to the merge block, passing it the block return value.
    //     self.builder.ins().jump(merge_block, &[else_return]);

    //     // Switch to the merge block for subsequent statements.
    //     self.builder.switch_to_block(merge_block);

    //     // We've now seen all the predecessors of the merge block.
    //     self.builder.seal_block(merge_block);

    //     // Read the value of the if-else by reading the merge block
    //     // parameter.
    //     let phi = self.builder.block_params(merge_block)[0];

    //     phi
    // }

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
    typ: types::Type,
    builder: &mut FunctionBuilder,
    exprs: &[Expr],
    _entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for expr in exprs {
        declare_variables_in_expr(typ, builder, &mut variables, &mut index, expr);
    }

    variables
}

/// Recursively descend through the AST, translating all implicit variable declarations.
fn declare_variables_in_expr(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    expr: &Expr,
) {
    if let Expr::Let(ident, _expr) = expr {
        let var = Variable::new(*index);
        if !variables.contains_key(&ident.name) {
            variables.insert(ident.name.to_owned(), var);
            builder.declare_var(var, int);
            *index += 1;
        }
    }

    expr.apply_children(|e| declare_variables_in_expr(int, builder, variables, index, e));
}
