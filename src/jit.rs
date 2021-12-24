use std::{collections::HashMap, slice, str::FromStr};

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

use crate::{
    intern::{Symbol, SymbolInterner},
    ir::{self, BinOp, Expr, ExprKind, Literal, LiteralKind},
    ty::{self, Ty, TyKind},
};

fn convert_type(ty: Ty<'_>) -> types::Type {
    match &ty.kind {
        TyKind::Int(int_ty) => match int_ty {
            // TODO: How do I determine this in general?
            ty::IntTy::ISize => cranelift::codegen::ir::types::I64,
            ty::IntTy::I8 => cranelift::codegen::ir::types::I8,
            ty::IntTy::I16 => cranelift::codegen::ir::types::I16,
            ty::IntTy::I32 => cranelift::codegen::ir::types::I32,
            ty::IntTy::I64 => cranelift::codegen::ir::types::I64,
        },
        TyKind::UInt(uint_ty) => match uint_ty {
            // TODO: How do I determine this in general?
            ty::UIntTy::USize => cranelift::codegen::ir::types::I64,
            ty::UIntTy::U8 => cranelift::codegen::ir::types::I8,
            ty::UIntTy::U16 => cranelift::codegen::ir::types::I16,
            ty::UIntTy::U32 => cranelift::codegen::ir::types::I32,
            ty::UIntTy::U64 => cranelift::codegen::ir::types::I64,
        },
        TyKind::Bool => cranelift::codegen::ir::types::B1,
        TyKind::Unit => todo!(),
        TyKind::Float(float_ty) => match float_ty {
            ty::FloatTy::F32 => cranelift::codegen::ir::types::F32,
            ty::FloatTy::F64 => cranelift::codegen::ir::types::F64,
        },
        TyKind::String => todo!(),
        TyKind::Tuple(_) => todo!(),
        TyKind::Struct(_) => todo!(),
    }
}

/// The basic JIT class.
pub struct JIT<'ctx> {
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

    symbols: &'ctx SymbolInterner,
}

impl<'ctx> JIT<'ctx> {
    pub fn new(symbols: &'ctx SymbolInterner) -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,

            symbols,
        }
    }

    // TODO: Make this accept an AST probably
    pub fn compile(&mut self, func: &ir::FnDef) -> anyhow::Result<*const u8> {
        self.translate(&func)?;

        // Next, declare the function to jit. Functions must be declared before they can be called,
        // or defined.
        //
        // TODO: This may be an area where the API should be streamlined; should we have a version
        // of `declare_function` that automatically declares the function?
        let func_name = self.symbols.resolve(&func.name.name);
        let id = self
            .module
            .declare_function(func_name, Linkage::Export, &self.ctx.func.signature)
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
    fn translate(&mut self, func: &ir::FnDef) -> anyhow::Result<()> {
        for (_name, typ) in &func.params {
            let ty = convert_type(typ.ty);
            self.ctx.func.signature.params.push(AbiParam::new(ty));
        }

        // Our toy language currently only supports one return value, though Cranelift is designed
        // to support more.
        let ty = convert_type(func.return_ty.ty);
        self.ctx.func.signature.returns.push(AbiParam::new(ty));

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

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            builder,
            variables: HashMap::new(),
            module: &mut self.module,
            symbols: self.symbols,
        };
        let return_value = trans.translate_block(func.return_ty.ty, &func.body);

        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}

/// A collection of state used for translating from toy-language AST nodes into Cranelift IR.
struct FunctionTranslator<'parent> {
    builder: FunctionBuilder<'parent>,
    variables: HashMap<Symbol, Variable>,
    module: &'parent mut JITModule,

    symbols: &'parent SymbolInterner,
}

impl<'a> FunctionTranslator<'a> {
    /// When you write out instructions in Cranelift, you get back `Value`s. You can then use these
    /// references in other instructions.
    fn translate_expr(&mut self, expr: &Expr) -> Value {
        match &expr.kind {
            ExprKind::Literal(lit) => self.translate_literal(lit),
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.translate_expr(lhs.as_ref());
                let rhs = self.translate_expr(rhs.as_ref());
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

            // ExprKind::Call(name, args) => self.translate_call(name, args),
            // ExprKind::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            ExprKind::Ident(ident) => {
                // `use_var` is used to read the value of a variable.
                let variable = self
                    .variables
                    .get(&ident.name)
                    .expect("variable not defined");
                self.builder.use_var(*variable)
            }

            ExprKind::Let(ident, expr) => {
                let var = Variable::new(self.variables.len());
                self.variables.insert(ident.name, var);
                let ty = convert_type(expr.ty);
                self.builder.declare_var(var, ty);

                let new_value = self.translate_expr(expr.as_ref());
                self.builder.def_var(var, new_value);
                new_value
            }
            ExprKind::Set(ident, expr) => {
                // `def_var` is used to write the value of a variable. Note that variables can have
                // multiple definitions. Cranelift will convert them into SSA form for itself
                // automatically.
                let new_value = self.translate_expr(expr.as_ref());
                let var = self.variables.get(&ident.name).unwrap();
                self.builder.def_var(*var, new_value);
                new_value
            }

            ExprKind::If { cond, then_expr, else_expr } => {
                self.translate_if_else(cond.as_ref(), then_expr.as_ref(), else_expr.as_deref())
            }

            ExprKind::Block(exprs) => self.translate_block(expr.ty, exprs),

            // ExprKind::WhileLoop(condition, loop_body) => {
            //     self.translate_while_loop(*condition, loop_body)
            // }
            _ => unimplemented!(),
        }
    }

    fn translate_literal(&mut self, lit: &Literal) -> Value {
        match &lit.kind {
            LiteralKind::Int(val) => {
                let ty = convert_type(lit.ty);
                self.builder.ins().iconst(ty, *val as i64)
            }
            LiteralKind::Bool(val) => {
                let ty = convert_type(lit.ty);
                self.builder.ins().bconst(ty, *val)
            }
            LiteralKind::Float(val) => {
                let input = self.symbols.resolve(val).replace('_', "");
                match lit.ty.kind {
                    // TODO: Move parsing logic somewhere else
                    TyKind::Float(ty::FloatTy::F32) => {
                        let val = f32::from_str(&input).unwrap();
                        self.builder.ins().f32const(val)
                    }
                    TyKind::Float(ty::FloatTy::F64) => {
                        let val = f64::from_str(&input).unwrap();
                        self.builder.ins().f64const(val)
                    }
                    _ => unreachable!(),
                }
            }
            LiteralKind::String(_) => todo!("Gotta figure out how data and pointers work"),
        }
        // ExprKind::Literal(Literal { kind: LiteralKind::Float(imm), .. }) => {
        //     self.builder.ins().f64const(*imm)
        // }
        // ExprKind::Literal(Literal { kind: LiteralKind::Int(imm), .. }) => {
        //     self.builder.ins().iconst(ASSUMED_TYPE, *imm as i64)
        // }
    }

    fn translate_if_else(
        &mut self,
        condition: &Expr,
        then_expr: &Expr,
        else_expr: Option<&Expr>,
    ) -> Value {
        let condition_value = self.translate_expr(condition);

        let ty = convert_type(then_expr.ty);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // If-else constructs in the toy language have a return value.
        // In traditional SSA form, this would produce a PHI between
        // the then and else bodies. Cranelift uses block parameters,
        // so set up a parameter in the merge block, and we'll pass
        // the return values to it from the branches.
        // TODO: We just assume that you return an int
        self.builder.append_block_param(merge_block, ty);

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

    fn translate_block(&mut self, ty: Ty, exprs: &[Expr]) -> Value {
        let ty = convert_type(ty);

        // Create a block which will have this value calculated
        let next_block = self.builder.create_block();
        self.builder.append_block_param(next_block, ty);

        // Translate all the expressions and keep the last one as the return value
        let mut rtn = self.builder.ins().iconst(ty, 0);
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
