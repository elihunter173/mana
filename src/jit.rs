use std::{collections::HashMap, slice, str::FromStr};

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

use crate::{
    intern::SymbolInterner,
    ir::{
        self,
        registry::{FunctionId, Registry, TypeId, VariableId},
    },
    ty::{self, TyKind, Type},
};

// TODO: The *Description naming scheme kinda sucks

// TODO: Check out cranelift-object

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
struct VariableDescription {
    vars: Vec<Variable>,
    type_desc: TypeDescription,
}

// TODO: This should maybe not be a vec and instead just a slice
#[derive(Debug)]
struct ValueDescription(Vec<Value>);

impl ValueDescription {
    fn unit() -> Self {
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
    functions: HashMap<ir::registry::FunctionId, cranelift_module::FuncId>,

    symbols: &'ctx SymbolInterner,
    registry: &'ctx Registry,
}

const DEFAULT_CALLCONV: isa::CallConv = isa::CallConv::Fast;

impl<'ctx> JIT<'ctx> {
    pub fn new(symbols: &'ctx SymbolInterner, registry: &'ctx Registry) -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
            functions: HashMap::new(),

            symbols,
            registry,
        }
    }
}

// Helpers
impl<'ctx> JIT<'ctx> {
    fn get_type_desc(&self, ty_id: TypeId) -> TypeDescription {
        TypeDescription::from_ty(self.registry.get_type(ty_id))
    }

    fn fill_signature(&self, func_sig: &ir::FunctionSignature, sig: &mut Signature) {
        for &var_id in &func_sig.params {
            let var = self.registry.get_variable(var_id);
            let type_desc = self.get_type_desc(var.type_id);
            sig.params.extend(type_desc.abi_params());
        }

        // Our toy language currently only supports one return value, though Cranelift is designed
        // to support more.
        let type_desc = self.get_type_desc(func_sig.return_ty.id);
        sig.returns.extend(type_desc.abi_params());
    }
}

impl<'ctx> JIT<'ctx> {
    pub fn compile(&mut self, module: &ir::Module) -> anyhow::Result<*const u8> {
        let mut main_func = None;

        // Go through and declare all items
        let mut signature = Signature::new(DEFAULT_CALLCONV);
        for item in &module.items {
            if let ir::Item::Function(func_id) = item {
                let (sig, _) = self.registry.get_function(*func_id);
                signature.clear(DEFAULT_CALLCONV);
                self.fill_signature(sig, &mut signature);
                let func_name = self.symbols.resolve(&sig.name.sym);
                let id = self
                    .module
                    .declare_function(func_name, Linkage::Export, &signature)
                    .unwrap();
                // TODO: I don't think this is a good way of doing this
                if func_name == "main" {
                    main_func = Some(id);
                }
                self.functions.insert(*func_id, id);
            }
        }
        let main_func = main_func.expect("need a main function. TODO: Support multi-file stuff");

        // Now define all items
        for item in &module.items {
            if let ir::Item::Function(func_id) = item {
                let (sig, body) = self.registry.get_function(*func_id);
                self.translate(sig, body.expect("undefined function"))?;
                self.module
                    .define_function(self.functions[func_id], &mut self.ctx)
                    .unwrap();

                self.module.clear_context(&mut self.ctx);
            }
        }

        // Finalize the functions which we just defined, which resolves any outstanding relocations
        // (patching in addresses, now that they're available).
        self.module.finalize_definitions();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(main_func);

        Ok(code)
    }

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
    fn translate(
        &mut self,
        sig: &ir::FunctionSignature,
        body: &ir::FunctionBody,
    ) -> anyhow::Result<()> {
        // TODO: This code is duplicated from JIT::fill_signature
        for &var_id in &sig.params {
            let var = self.registry.get_variable(var_id);
            let type_desc = self.get_type_desc(var.type_id);
            self.ctx
                .func
                .signature
                .params
                .extend(type_desc.abi_params());
        }

        // Our toy language currently only supports one return value, though Cranelift is designed
        // to support more.
        let type_desc = self.get_type_desc(sig.return_ty.id);
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

        // Define variables for function parameters
        let mut variables = HashMap::new();
        let mut param_i = 0;
        // TODO: This is copied and modified from declare_variable
        for &var_id in &sig.params {
            let var = self.registry.get_variable(var_id);
            let type_desc = TypeDescription::from_ty(self.registry.get_type(var.type_id));

            let mut cg_vars = Vec::with_capacity(type_desc.0.len());
            let cg_values =
                builder.block_params(entry_block)[param_i..param_i + type_desc.0.len()].to_owned();
            param_i += type_desc.0.len();
            for (&ty, &value) in type_desc.0.iter().zip(&cg_values) {
                let var = Variable::new(variables.len() + cg_vars.len());
                builder.declare_var(var, ty);
                builder.def_var(var, value);
                cg_vars.push(var);
            }

            variables.insert(
                var_id,
                VariableDescription { vars: cg_vars.to_owned(), type_desc },
            );
        }

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            builder,
            variables,
            module: &mut self.module,
            functions: &self.functions,
            symbols: self.symbols,
            registry: self.registry,
        };
        let return_value = trans.translate_block(&body.exprs);

        // Emit the return instruction.
        trans.builder.ins().return_(&return_value.0);

        // TODO: You can do this to emit the IR. I should have a command line option to emit IR
        // dbg!(&trans.builder.func);

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

    functions: &'parent HashMap<ir::registry::FunctionId, cranelift_module::FuncId>,

    symbols: &'parent SymbolInterner,
    registry: &'parent Registry,
}

// TODO: This is duplicated from JIT. Find a way to fix that
// Helpers
impl<'parent> FunctionTranslator<'parent> {
    fn get_type_desc(&self, ty_id: TypeId) -> TypeDescription {
        TypeDescription::from_ty(self.registry.get_type(ty_id))
    }

    fn declare_variable(
        &mut self,
        var_id: VariableId,
        type_desc: TypeDescription,
        value_desc: ValueDescription,
    ) {
        let mut vars = Vec::with_capacity(type_desc.0.len());
        for (&ty, &value) in type_desc.0.iter().zip(value_desc.0.iter()) {
            let var = Variable::new(self.variables.len() + vars.len());
            self.builder.declare_var(var, ty);
            self.builder.def_var(var, value);
            vars.push(var);
        }

        self.variables
            .insert(var_id, VariableDescription { vars, type_desc });
    }
}

impl<'a> FunctionTranslator<'a> {
    /// When you write out instructions in Cranelift, you get back `Value`s. You can then use these
    /// references in other instructions.
    fn translate_expr(&mut self, expr: &ir::Expr) -> ValueDescription {
        match &expr.kind {
            ir::ExprKind::Literal(lit) => self.translate_literal(lit),
            ir::ExprKind::Binary(op, lhs, rhs) => {
                self.translate_binary(op, lhs.as_ref(), rhs.as_ref())
            }

            // ExprKind::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            ir::ExprKind::Variable(id) => {
                // `use_var` is used to read the value of a variable.
                let variable_desc = self.variables.get(&id).expect("variable not defined");
                ValueDescription(
                    variable_desc
                        .vars
                        .iter()
                        .map(|var| self.builder.use_var(*var))
                        .collect(),
                )
            }

            ir::ExprKind::Let(var_id, init_expr) => self.translate_let(*var_id, init_expr),

            ir::ExprKind::Set(var_id, expr) => {
                let value_desc = self.translate_expr(expr.as_ref());
                let var_desc = self.variables.get(var_id).unwrap();
                for (&var, &value) in var_desc.vars.iter().zip(value_desc.0.iter()) {
                    self.builder.def_var(var, value);
                }
                ValueDescription::unit()
            }

            ir::ExprKind::If { cond, then_expr, else_expr } => {
                self.translate_if_else(cond.as_ref(), then_expr.as_ref(), else_expr.as_deref())
            }

            ir::ExprKind::Block(exprs) => self.translate_block(exprs),

            // ExprKind::WhileLoop(condition, loop_body) => {
            //     self.translate_while_loop(*condition, loop_body)
            // }
            ir::ExprKind::FnCall(func, args) => self.translate_call(*func, args),

            _ => todo!("{:?}", expr),
        }
    }

    fn translate_literal(&mut self, lit: &ir::Literal) -> ValueDescription {
        match &lit.kind {
            ir::LiteralKind::Int(val) => {
                if let &[ty] = self.get_type_desc(lit.ty).0.as_slice() {
                    ValueDescription(Vec::from([self.builder.ins().iconst(ty, *val as i64)]))
                } else {
                    panic!("I think this should be impossible");
                }
            }
            ir::LiteralKind::Bool(val) => {
                if let &[ty] = self.get_type_desc(lit.ty).0.as_slice() {
                    ValueDescription(Vec::from([self.builder.ins().bconst(ty, *val)]))
                } else {
                    panic!("I think this should be impossible");
                }
            }
            ir::LiteralKind::Float(val) => {
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
            ir::LiteralKind::String(_) => todo!("Gotta figure out how data and pointers work"),
        }
    }

    fn translate_binary(
        &mut self,
        op: &ir::BinOp,
        lhs: &ir::Expr,
        rhs: &ir::Expr,
    ) -> ValueDescription {
        // Special handling of short-circuiting operations
        if let ir::BinOp::Lor = op {
            // Create a block which will have this value calculated
            let else_block = self.builder.create_block();
            let merge_block = self.builder.create_block();
            // TODO: This bool value should be using my registry.bool() type
            self.builder.append_block_param(merge_block, types::B1);

            let lhs = self.translate_expr(lhs).0[0];
            self.builder.ins().brnz(lhs, merge_block, &[lhs]);
            self.builder.ins().jump(else_block, &[]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);
            let rhs = self.translate_expr(rhs).0[0];
            self.builder.ins().jump(merge_block, &[rhs]);

            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);
            return ValueDescription(Vec::from(self.builder.block_params(merge_block)));
        }

        if let ir::BinOp::Land = op {
            // Create a block which will have this value calculated
            let else_block = self.builder.create_block();
            let merge_block = self.builder.create_block();
            // TODO: This bool value should be using my registry.bool() type
            self.builder.append_block_param(merge_block, types::B1);

            let lhs = self.translate_expr(lhs).0[0];
            self.builder.ins().brz(lhs, merge_block, &[lhs]);
            self.builder.ins().jump(else_block, &[]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);
            let rhs = self.translate_expr(rhs).0[0];
            self.builder.ins().jump(merge_block, &[rhs]);

            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);
            return ValueDescription(Vec::from(self.builder.block_params(merge_block)));
        }

        // TODO: This indexing is a hack
        // TODO: Look at the type to determine how to implement the function
        let lhs = self.translate_expr(lhs).0[0];
        let rhs = self.translate_expr(rhs).0[0];
        let ins = self.builder.ins();
        // TODO: Do signed vs unsigned operations based off type
        let val = match op {
            ir::BinOp::Add => ins.iadd(lhs, rhs),
            ir::BinOp::Sub => ins.isub(lhs, rhs),
            ir::BinOp::Mul => ins.imul(lhs, rhs),
            // TODO: Do sdiv or udiv depending on sign
            ir::BinOp::Div => ins.sdiv(lhs, rhs),
            ir::BinOp::Rem => ins.srem(lhs, rhs),

            ir::BinOp::Eq => ins.icmp(IntCC::Equal, lhs, rhs),
            ir::BinOp::Neq => ins.icmp(IntCC::NotEqual, lhs, rhs),
            ir::BinOp::Lt => ins.icmp(IntCC::SignedLessThan, lhs, rhs),
            ir::BinOp::Leq => ins.icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
            ir::BinOp::Gt => ins.icmp(IntCC::SignedGreaterThan, lhs, rhs),
            ir::BinOp::Geq => ins.icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),

            ir::BinOp::Lor | ir::BinOp::Land => unreachable!("this is handled above"),
        };
        ValueDescription(Vec::from([val]))
    }

    fn translate_if_else(
        &mut self,
        condition: &ir::Expr,
        then_expr: &ir::Expr,
        else_expr: Option<&ir::Expr>,
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
            // I don't need to pass a type here because we're guaranteed that the block returns
            // nothing if there's no else expression.
            // TODO: This needs to be double checked
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

    fn translate_let(&mut self, var_id: VariableId, expr: &ir::Expr) -> ValueDescription {
        let type_desc = self.get_type_desc(expr.ty);
        let value_desc = self.translate_expr(expr);
        self.declare_variable(var_id, type_desc, value_desc);
        ValueDescription::unit()
    }

    fn translate_block(&mut self, exprs: &[ir::Expr]) -> ValueDescription {
        // TODO: I think I can get away with not generating blocks here
        // Create a block which will have this value calculated

        // Translate all the expressions and keep the last one as the return value
        let mut rtn = None;
        for expr in exprs {
            rtn = Some(self.translate_expr(expr));
        }

        // Return the value of this block
        rtn.unwrap_or(ValueDescription::unit())
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

    fn translate_call(&mut self, func_id: FunctionId, args: &[ir::Expr]) -> ValueDescription {
        let local_callee = self
            .module
            .declare_func_in_func(self.functions[&func_id], &mut self.builder.func);

        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.extend(self.translate_expr(arg).0)
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        // TODO: Why the hell do I have to just take the first one??
        ValueDescription(self.builder.inst_results(call).to_vec())
    }

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
