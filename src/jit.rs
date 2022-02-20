use std::{collections::HashMap, slice, str::FromStr};

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

use crate::{
    intern::SymbolInterner,
    ir::{
        self,
        registry::{Registry, TypeId, VariableId},
        BinOp, Expr, ExprKind, Literal, LiteralKind,
    },
    ty::{self, TyKind, Type},
};

// TODO: The *Description naming scheme kinda sucks

#[derive(Debug)]
struct TypeDescription(Vec<types::Type>);

impl TypeDescription {
    fn from_ty(ty: &Type) -> Self {
        // TODO: How do I determine the I/USize in general?
        match &ty.kind {
            TyKind::Int(int_ty) => match int_ty {
                ty::IntTy::ISize => Self(Vec::from([types::I64])),
                ty::IntTy::I8 => Self(Vec::from([types::I8])),
                ty::IntTy::I16 => Self(Vec::from([types::I16])),
                ty::IntTy::I32 => Self(Vec::from([types::I32])),
                ty::IntTy::I64 => Self(Vec::from([types::I64])),
            },
            TyKind::UInt(uint_ty) => match uint_ty {
                ty::UIntTy::USize => Self(Vec::from([types::I64])),
                ty::UIntTy::U8 => Self(Vec::from([types::I8])),
                ty::UIntTy::U16 => Self(Vec::from([types::I16])),
                ty::UIntTy::U32 => Self(Vec::from([types::I32])),
                ty::UIntTy::U64 => Self(Vec::from([types::I64])),
            },
            TyKind::Bool => Self(Vec::from([types::B1])),
            TyKind::Float(float_ty) => match float_ty {
                ty::FloatTy::F32 => Self(Vec::from([types::F32])),
                ty::FloatTy::F64 => Self(Vec::from([types::F64])),
            },
            TyKind::String => Self(Vec::from([types::R64, types::I64])),
            TyKind::Tuple(types) => Self(
                types
                    .iter()
                    .map(|ty| TypeDescription::from_ty(ty).0)
                    .flatten()
                    .collect(),
            ),
            TyKind::Struct(_) => todo!(),
        }
    }

    fn abi_params(&self) -> Vec<AbiParam> {
        self.0.iter().map(|ty| AbiParam::new(*ty)).collect()
    }

    fn types(&self) -> &[types::Type] {
        &self.0
    }
}

#[derive(Debug)]
struct VariableDescription(Vec<Variable>);

#[derive(Debug)]
struct ValueDescription(Vec<Value>);

impl ValueDescription {
    fn empty() -> Self {
        Self(Vec::new())
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

    // My Stuff
    symbols: &'ctx SymbolInterner,
    registry: &'ctx Registry,
}

impl<'ctx> JIT<'ctx> {
    pub fn new(symbols: &'ctx SymbolInterner, registry: &'ctx Registry) -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,

            symbols,
            registry,
        }
    }

    pub fn compile(&mut self, module: &ir::Module) -> anyhow::Result<*const u8> {
        let func = if let &[ir::Item::Function(func)] = module.items.as_slice() {
            self.registry.get_function(func)
        } else {
            panic!("only support a single function right now");
        };

        self.translate(func)?;

        // Next, declare the function to jit. Functions must be declared before they can be called,
        // or defined.
        let func_name = self.symbols.resolve(&func.name.sym);
        let id = self
            .module
            .declare_function(func_name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        // Define the function to jit. This finishes compilation, although there may be outstanding
        // relocations to perform. Currently, jit cannot finish relocations until all functions to
        // be called are defined. For this toy demo for now, we'll just finalize the function
        // below.
        self.module.define_function(id, &mut self.ctx).unwrap();

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any outstanding relocations
        // (patching in addresses, now that they're available).
        self.module.finalize_definitions();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }
}

// Helpers
impl<'ctx> JIT<'ctx> {
    fn get_type_desc(&self, ty_id: TypeId) -> TypeDescription {
        TypeDescription::from_ty(self.registry.get_type(ty_id))
    }
}

impl<'ctx> JIT<'ctx> {
    /// Create a zero-initialized data section.
    fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
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
        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    }

    // Translate from toy-language AST nodes into Cranelift IR.
    fn translate(&mut self, func: &ir::Function) -> anyhow::Result<()> {
        for (_name, ty) in &func.params {
            let type_desc = self.get_type_desc(ty.id);
            self.ctx
                .func
                .signature
                .params
                .extend(type_desc.abi_params());
        }

        // Our toy language currently only supports one return value, though Cranelift is designed
        // to support more.
        let type_desc = self.get_type_desc(func.return_ty.id);
        self.ctx
            .func
            .signature
            .returns
            .extend(type_desc.abi_params());

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to the function's
        // parameters.
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
            registry: self.registry,
        };
        let return_value = trans.translate_block(func.return_ty.id, &func.body);

        // Emit the return instruction.
        trans.builder.ins().return_(&return_value.0);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}

/// A collection of state used for translating from toy-language AST nodes into Cranelift IR.
struct FunctionTranslator<'parent> {
    builder: FunctionBuilder<'parent>,
    variables: HashMap<VariableId, VariableDescription>,
    module: &'parent mut JITModule,

    symbols: &'parent SymbolInterner,
    registry: &'parent Registry,
}

// TODO: This is duplicated from JIT. Find a way to fix that
// Helpers
impl<'parent> FunctionTranslator<'parent> {
    fn get_type_desc(&self, ty_id: TypeId) -> TypeDescription {
        TypeDescription::from_ty(self.registry.get_type(ty_id))
    }
}

impl<'a> FunctionTranslator<'a> {
    /// When you write out instructions in Cranelift, you get back `Value`s. You can then use these
    /// references in other instructions.
    fn translate_expr(&mut self, expr: &Expr) -> ValueDescription {
        match &expr.kind {
            ExprKind::Literal(lit) => self.translate_literal(lit),
            ExprKind::Binary(op, lhs, rhs) => {
                // TODO: This indexing is a hack
                // TODO: Look at the type to determine how to implement the function
                let lhs = self.translate_expr(lhs.as_ref()).0[0];
                let rhs = self.translate_expr(rhs.as_ref()).0[0];
                let ins = self.builder.ins();
                let val = match op {
                    BinOp::Add => ins.iadd(lhs, rhs),
                    BinOp::Sub => ins.isub(lhs, rhs),
                    BinOp::Mul => ins.imul(lhs, rhs),
                    BinOp::Div => ins.sdiv(lhs, rhs),
                    BinOp::Eq => ins.icmp(IntCC::Equal, lhs, rhs),
                    BinOp::Neq => ins.icmp(IntCC::NotEqual, lhs, rhs),
                    _ => unimplemented!(),
                };
                ValueDescription(Vec::from([val]))
            }

            // ExprKind::Call(name, args) => self.translate_call(name, args),
            // ExprKind::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            ExprKind::Variable(id) => {
                // `use_var` is used to read the value of a variable.
                let variable_desc = self.variables.get(&id).expect("variable not defined");
                ValueDescription(
                    variable_desc
                        .0
                        .iter()
                        .map(|var| self.builder.use_var(*var))
                        .collect(),
                )
            }

            ExprKind::Let(var_id, init_expr) => {
                // TODO: This is a nightmare to maintain. See if I can clean up the way it works
                let type_desc = self.get_type_desc(init_expr.ty);
                let mut var_vec = Vec::with_capacity(type_desc.0.len());
                let value_desc = self.translate_expr(init_expr.as_ref());
                for (&ty, &value) in type_desc.0.iter().zip(value_desc.0.iter()) {
                    let var = Variable::new(self.variables.len() + var_vec.len());
                    self.builder.declare_var(var, ty);
                    self.builder.def_var(var, value);
                    var_vec.push(var);
                }
                self.variables.insert(*var_id, VariableDescription(var_vec));
                ValueDescription::empty()
            }

            ExprKind::Set(var_id, expr) => {
                let value_desc = self.translate_expr(expr.as_ref());
                let var_desc = self.variables.get(var_id).unwrap();
                for (&var, &value) in var_desc.0.iter().zip(value_desc.0.iter()) {
                    self.builder.def_var(var, value);
                }
                ValueDescription::empty()
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

    fn translate_literal(&mut self, lit: &Literal) -> ValueDescription {
        match &lit.kind {
            LiteralKind::Int(val) => {
                if let &[ty] = self.get_type_desc(lit.ty).0.as_slice() {
                    ValueDescription(Vec::from([self.builder.ins().iconst(ty, *val as i64)]))
                } else {
                    panic!("I think this should be impossible");
                }
            }
            LiteralKind::Bool(val) => {
                if let &[ty] = self.get_type_desc(lit.ty).0.as_slice() {
                    ValueDescription(Vec::from([self.builder.ins().bconst(ty, *val)]))
                } else {
                    panic!("I think this should be impossible");
                }
            }
            LiteralKind::Float(val) => {
                let input = self.symbols.resolve(val);
                match self.registry.get_type(lit.ty).kind {
                    // TODO: Move parsing logic somewhere else
                    TyKind::Float(ty::FloatTy::F32) => {
                        let val = f32::from_str(&input).unwrap();
                        ValueDescription(Vec::from([self.builder.ins().f32const(val)]))
                    }
                    TyKind::Float(ty::FloatTy::F64) => {
                        let val = f64::from_str(&input).unwrap();
                        ValueDescription(Vec::from([self.builder.ins().f64const(val)]))
                    }
                    _ => unreachable!(),
                }
            }
            LiteralKind::String(_) => todo!("Gotta figure out how data and pointers work"),
        }
    }

    fn translate_if_else(
        &mut self,
        condition: &Expr,
        then_expr: &Expr,
        else_expr: Option<&Expr>,
    ) -> ValueDescription {
        let condition_value = self.translate_expr(condition);

        let type_desc = self.get_type_desc(then_expr.ty);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        for &ty in type_desc.types() {
            self.builder.append_block_param(merge_block, ty);
        }

        // Test the if condition and conditionally branch.
        if else_expr.is_some() {
            let val = condition_value.0[0];
            self.builder.ins().brz(val, else_block, &[]);
        } else {
            let val = condition_value.0[0];
            self.builder.ins().brz(val, merge_block, &[]);
        }
        // Fall through to then block.
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_return = self.translate_expr(then_expr);

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &then_return.0);

        if let Some(else_expr) = else_expr {
            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);
            let else_return = self.translate_expr(else_expr);

            // Jump to the merge block, passing it the block return value.
            self.builder.ins().jump(merge_block, &else_return.0);
        }

        // Switch to the merge block for subsequent statements.
        self.builder.switch_to_block(merge_block);

        // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);

        // Read the value of the if-else by reading the merge block
        // parameter.
        ValueDescription(Vec::from(self.builder.block_params(merge_block)))
    }

    fn translate_block(&mut self, ty: TypeId, exprs: &[Expr]) -> ValueDescription {
        let type_desc = self.get_type_desc(ty);

        // Create a block which will have this value calculated
        let next_block = self.builder.create_block();
        for &ty in type_desc.types() {
            self.builder.append_block_param(next_block, ty);
        }

        // Translate all the expressions and keep the last one as the return value
        let mut rtn = None;
        for expr in exprs {
            rtn = Some(self.translate_expr(expr));
        }

        // Switch to the next block passing in the return
        self.builder
            .ins()
            .jump(next_block, &rtn.expect("this must be used").0);
        self.builder.switch_to_block(next_block);
        self.builder.seal_block(next_block);

        // Return the value of this block
        ValueDescription(Vec::from(self.builder.block_params(next_block)))
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
