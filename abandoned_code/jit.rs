use std::{collections::HashMap, ops::ControlFlow, slice, str::FromStr};

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

// TODO: Check out cranelift-object for generating .o ELF objects

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
            TyKind::Struct(_) => todo!("struct types"),
        }
    }

    fn abi_params(&self) -> Vec<AbiParam> {
        self.0.iter().map(|ty| AbiParam::new(*ty)).collect()
    }

    fn as_types(&self) -> &[types::Type] {
        self.0.as_slice()
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

    fn as_values(&self) -> &[Value] {
        self.0.as_slice()
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
    pub fn compile(&mut self, module: &ir::Module) -> *const u8 {
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
                self.translate(sig, body.expect("undefined function"));
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

        code
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
    fn translate(&mut self, sig: &ir::FunctionSignature, body: &ir::FunctionBody) {
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
            loop_info: Vec::new(),

            symbols: self.symbols,
            registry: self.registry,
        };
        // If we have ControlFlow::Break then we ended with a return
        if let ControlFlow::Continue(return_value) = trans.translate_block(&body.exprs) {
            trans.builder.ins().return_(return_value.as_values());
        }

        // TODO: You can do this to emit the IR. I should have a command line option to emit IR
        // dbg!(&trans.builder.func);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
    }
}

#[derive(Debug)]
struct LoopInfo {
    body_block: Block,
    exit_block: Block,
}

/// A collection of state used for translating from toy-language AST nodes into Cranelift IR.
struct FunctionTranslator<'parent> {
    builder: FunctionBuilder<'parent>,
    variables: HashMap<VariableId, VariableDescription>,
    module: &'parent mut JITModule,

    functions: &'parent HashMap<ir::registry::FunctionId, cranelift_module::FuncId>,

    loop_info: Vec<LoopInfo>,

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
    fn translate_expr(&mut self, expr: &ir::Expr) -> ControlFlow<(), ValueDescription> {
        match &expr.kind {
            ir::ExprKind::Literal(lit) => self.translate_literal(lit),
            ir::ExprKind::Binary(op, lhs, rhs) => {
                self.translate_binary(op, lhs.as_ref(), rhs.as_ref())
            }
            ir::ExprKind::Unary(op, expr) => self.translate_unary(op, expr.as_ref()),

            // ExprKind::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            ir::ExprKind::Variable(id) => {
                // `use_var` is used to read the value of a variable.
                let variable_desc = self.variables.get(&id).expect("variable not defined");
                ControlFlow::Continue(ValueDescription(
                    variable_desc
                        .vars
                        .iter()
                        .map(|var| self.builder.use_var(*var))
                        .collect(),
                ))
            }

            ir::ExprKind::Let(var_id, init_expr) => self.translate_let(*var_id, init_expr),
            ir::ExprKind::Set(var_id, expr) => self.translate_set(*var_id, expr),

            ir::ExprKind::If { cond, then_expr, else_expr } => {
                self.translate_if_else(cond.as_ref(), then_expr.as_ref(), else_expr.as_deref())
            }

            ir::ExprKind::Block(exprs) => self.translate_block(exprs),

            // ExprKind::WhileLoop(condition, loop_body) => {
            //     self.translate_while_loop(*condition, loop_body)
            // }
            ir::ExprKind::FnCall(func, args) => self.translate_call(*func, args),

            ir::ExprKind::Loop(expr) => self.translate_loop(expr),
            ir::ExprKind::Break(expr) => self.translate_break(expr),
            ir::ExprKind::Continue(expr) => self.translate_continue(expr),
            ir::ExprKind::Return(expr) => self.translate_return(expr),
        }
    }

    fn translate_literal(&mut self, lit: &ir::Literal) -> ControlFlow<(), ValueDescription> {
        match &lit.kind {
            ir::LiteralKind::Int(val) => {
                if let &[ty] = self.get_type_desc(lit.ty).0.as_slice() {
                    let cg_val = self.builder.ins().iconst(ty, *val as i64);
                    ControlFlow::Continue(ValueDescription(Vec::from([cg_val])))
                } else {
                    panic!("I think this should be impossible");
                }
            }
            ir::LiteralKind::Bool(val) => {
                if let &[ty] = self.get_type_desc(lit.ty).0.as_slice() {
                    let cg_val = self.builder.ins().bconst(ty, *val);
                    ControlFlow::Continue(ValueDescription(Vec::from([cg_val])))
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
                        let cg_val = self.builder.ins().f32const(val);
                        ControlFlow::Continue(ValueDescription(Vec::from([cg_val])))
                    }
                    TyKind::Float(ty::FloatTy::F64) => {
                        let val = f64::from_str(&input).unwrap();
                        let cg_val = self.builder.ins().f64const(val);
                        ControlFlow::Continue(ValueDescription(Vec::from([cg_val])))
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
    ) -> ControlFlow<(), ValueDescription> {
        // Special handling of short-circuiting operations
        if let ir::BinOp::Lor = op {
            // Create a block which will have this value calculated
            let else_block = self.builder.create_block();
            let merge_block = self.builder.create_block();
            // TODO: This bool value should be using my registry.bool() type?
            self.builder.append_block_param(merge_block, types::B1);

            if let ControlFlow::Continue(lhs) = self.translate_expr(lhs) {
                self.builder
                    .ins()
                    .brnz(lhs.0[0], merge_block, lhs.as_values());
                self.builder.ins().jump(else_block, &[]);
            }

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);
            if let ControlFlow::Continue(rhs) = self.translate_expr(rhs) {
                self.builder.ins().jump(merge_block, rhs.as_values());
            }

            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);
            let cg_vals = self.builder.block_params(merge_block);
            return ControlFlow::Continue(ValueDescription(Vec::from(cg_vals)));
        }

        if let ir::BinOp::Land = op {
            // Create a block which will have this value calculated
            let else_block = self.builder.create_block();
            let merge_block = self.builder.create_block();
            // TODO: This bool value should be using my registry.bool() type?
            self.builder.append_block_param(merge_block, types::B1);

            if let ControlFlow::Continue(lhs) = self.translate_expr(lhs) {
                self.builder
                    .ins()
                    .brz(lhs.0[0], merge_block, lhs.as_values());
                self.builder.ins().jump(else_block, &[]);
            }

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);
            if let ControlFlow::Continue(rhs) = self.translate_expr(rhs) {
                self.builder.ins().jump(merge_block, rhs.as_values());
            }

            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);
            let cg_vals = self.builder.block_params(merge_block);
            return ControlFlow::Continue(ValueDescription(Vec::from(cg_vals)));
        }

        // TODO: This indexing is a hack
        // TODO: Look at the type to determine how to implement the function
        let lhs = self.translate_expr(lhs)?.0[0];
        let rhs = self.translate_expr(rhs)?.0[0];
        let ins = self.builder.ins();
        // TODO: Do signed vs unsigned operations based off type
        let val = match op {
            ir::BinOp::Add => ins.iadd(lhs, rhs),
            ir::BinOp::Sub => ins.isub(lhs, rhs),
            ir::BinOp::Mul => ins.imul(lhs, rhs),
            ir::BinOp::Div => ins.sdiv(lhs, rhs),
            ir::BinOp::Rem => ins.srem(lhs, rhs),
            ir::BinOp::Band => ins.band(lhs, rhs),
            ir::BinOp::Bor => ins.bor(lhs, rhs),
            ir::BinOp::Bxor => ins.bxor(lhs, rhs),

            ir::BinOp::Eq => ins.icmp(IntCC::Equal, lhs, rhs),
            ir::BinOp::Neq => ins.icmp(IntCC::NotEqual, lhs, rhs),

            ir::BinOp::Lt => ins.icmp(IntCC::SignedLessThan, lhs, rhs),
            ir::BinOp::Leq => ins.icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
            ir::BinOp::Gt => ins.icmp(IntCC::SignedGreaterThan, lhs, rhs),
            ir::BinOp::Geq => ins.icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),

            ir::BinOp::Lor | ir::BinOp::Land => unreachable!("this is handled above"),
        };
        ControlFlow::Continue(ValueDescription(Vec::from([val])))
    }

    fn translate_unary(
        &mut self,
        op: &ir::UnaryOp,
        expr: &ir::Expr,
    ) -> ControlFlow<(), ValueDescription> {
        let val = match op {
            ir::UnaryOp::Neg => {
                let val = self.translate_expr(expr)?;
                self.builder.ins().ineg(val.0[0])
            }

            ir::UnaryOp::Lnot => {
                let val = self.translate_expr(expr)?;
                self.builder.ins().bnot(val.0[0])
            }

            ir::UnaryOp::Bnot => {
                let val = self.translate_expr(expr)?;
                self.builder.ins().bnot(val.0[0])
            }
        };
        ControlFlow::Continue(ValueDescription(Vec::from([val])))
    }

    fn translate_if_else(
        &mut self,
        condition: &ir::Expr,
        then_expr: &ir::Expr,
        else_expr: Option<&ir::Expr>,
    ) -> ControlFlow<(), ValueDescription> {
        let type_desc = self.get_type_desc(then_expr.ty);

        let then_block = self.builder.create_block();
        let else_info = if let Some(else_expr) = else_expr {
            Some((self.builder.create_block(), else_expr))
        } else {
            None
        };

        let merge_block = self.builder.create_block();

        for &ty in type_desc.as_types() {
            self.builder.append_block_param(merge_block, ty);
        }

        if let ControlFlow::Continue(condition_value) = self.translate_expr(condition) {
            // Test the if condition and conditionally branch.
            let val = condition_value.0[0];
            if let Some((else_block, _)) = else_info {
                self.builder.ins().brz(val, else_block, &[]);
            } else {
                // I don't need to pass a type here because we're guaranteed that the block returns
                // nothing if there's no else expression.
                self.builder.ins().brz(val, merge_block, &[]);
            }
            // Fall through to then block.
            self.builder.ins().jump(then_block, &[]);
        }

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        if let ControlFlow::Continue(then_return) = self.translate_expr(then_expr) {
            self.builder
                .ins()
                .jump(merge_block, then_return.as_values());
        }

        if let Some((else_block, else_expr)) = else_info {
            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            if let ControlFlow::Continue(else_return) = self.translate_expr(else_expr) {
                self.builder
                    .ins()
                    .jump(merge_block, else_return.as_values());
            }
        }

        // Switch to the merge block for subsequent statements.
        self.builder.switch_to_block(merge_block);
        // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);

        // Read the value of the if-else by reading the merge block
        // parameter.
        let cg_vals = self.builder.block_params(merge_block);
        ControlFlow::Continue(ValueDescription(Vec::from(cg_vals)))
    }

    fn translate_let(
        &mut self,
        var_id: VariableId,
        expr: &ir::Expr,
    ) -> ControlFlow<(), ValueDescription> {
        let type_desc = self.get_type_desc(expr.ty);
        let value_desc = self.translate_expr(expr)?;
        self.declare_variable(var_id, type_desc, value_desc);
        ControlFlow::Continue(ValueDescription::unit())
    }

    fn translate_set(
        &mut self,
        var_id: VariableId,
        expr: &ir::Expr,
    ) -> ControlFlow<(), ValueDescription> {
        let value_desc = self.translate_expr(expr)?;
        let var_desc = self.variables.get(&var_id).unwrap();
        for (&var, &value) in var_desc.vars.iter().zip(value_desc.0.iter()) {
            self.builder.def_var(var, value);
        }
        ControlFlow::Continue(ValueDescription::unit())
    }

    fn translate_block(&mut self, exprs: &[ir::Expr]) -> ControlFlow<(), ValueDescription> {
        // Translate all the expressions and keep the last one as the return value
        let mut rtn = None;
        for expr in exprs {
            rtn = Some(self.translate_expr(expr)?);
        }

        // Return the value of this block
        ControlFlow::Continue(rtn.unwrap_or(ValueDescription::unit()))
    }

    fn translate_call(
        &mut self,
        func_id: FunctionId,
        args: &[ir::Expr],
    ) -> ControlFlow<(), ValueDescription> {
        let local_callee = self
            .module
            .declare_func_in_func(self.functions[&func_id], &mut self.builder.func);

        let mut arg_values = Vec::new();
        for arg in args {
            let expr = self.translate_expr(arg)?;
            arg_values.extend(expr.as_values())
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        ControlFlow::Continue(ValueDescription(self.builder.inst_results(call).to_vec()))
    }

    fn translate_loop(&mut self, expr: &ir::Expr) -> ControlFlow<(), ValueDescription> {
        let loop_type_desc = self.get_type_desc(expr.ty);

        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        for &ty in loop_type_desc.as_types() {
            self.builder.append_block_param(exit_block, ty);
        }

        self.builder.ins().jump(body_block, &[]);
        self.builder.switch_to_block(body_block);

        self.loop_info.push(LoopInfo { body_block, exit_block });
        if let ControlFlow::Continue(_val) = self.translate_expr(expr) {
            self.builder.ins().jump(body_block, &[]);
        }
        self.builder.seal_block(body_block);
        self.loop_info.pop().unwrap();

        // TODO: I should register parameters of the break type with the exit_block
        self.builder.switch_to_block(exit_block);
        self.builder.seal_block(exit_block);
        let cg_vals = self.builder.block_params(exit_block);
        ControlFlow::Continue(ValueDescription(Vec::from(cg_vals)))
    }

    fn translate_break(
        &mut self,
        expr: &Option<Box<ir::Expr>>,
    ) -> ControlFlow<(), ValueDescription> {
        let exit_block = self
            .loop_info
            .last()
            .expect("break used outside loop")
            .exit_block;
        if let Some(expr) = expr {
            let val = self.translate_expr(expr)?;
            self.builder.ins().jump(exit_block, val.as_values());
        } else {
            self.builder.ins().jump(exit_block, &[]);
        }
        ControlFlow::Break(())
    }

    fn translate_continue(
        &mut self,
        expr: &Option<Box<ir::Expr>>,
    ) -> ControlFlow<(), ValueDescription> {
        let body_block = self
            .loop_info
            .last()
            .expect("continue used outside loop")
            .body_block;
        if let Some(expr) = expr {
            let val = self.translate_expr(expr)?;
            self.builder.ins().jump(body_block, val.as_values());
        } else {
            self.builder.ins().jump(body_block, &[]);
        }
        ControlFlow::Break(())
    }

    fn translate_return(
        &mut self,
        expr: &Option<Box<ir::Expr>>,
    ) -> ControlFlow<(), ValueDescription> {
        if let Some(expr) = expr {
            let val = self.translate_expr(expr)?;
            self.builder.ins().return_(val.as_values());
        } else {
            self.builder.ins().return_(&[]);
        }
        ControlFlow::Break(())
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
