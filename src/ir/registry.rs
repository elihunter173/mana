use crate::{
    ast::Ident,
    ir::{Expr, Variable},
    ty::Type,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DefId(usize);

#[derive(Debug)]
pub struct Registry {
    types: Vec<Type>,
    variables: Vec<Variable>,
    defs: Vec<(Ident, Option<Expr>)>,
}

impl Registry {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            variables: Vec::new(),
            defs: Vec::new(),
        }
    }

    pub fn get_type(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    pub fn register_type(&mut self, ty: Type) -> TypeId {
        self.types.push(ty);
        TypeId(self.types.len() - 1)
    }

    pub fn get_variable(&self, id: VariableId) -> &Variable {
        &self.variables[id.0]
    }

    pub fn register_variable(&mut self, var: Variable) -> VariableId {
        self.variables.push(var);
        VariableId(self.variables.len() - 1)
    }

    pub fn declare_def(&mut self, name: Ident) -> DefId {
        self.defs.push((name, None));
        DefId(self.defs.len() - 1)
    }

    pub fn define_def(&mut self, def: DefId, value: Expr) {
        self.defs[def.0].1 = Some(value);
    }

    pub fn get_def(&self, id: DefId) -> (&Ident, Option<&Expr>) {
        let (name, value) = &self.defs[id.0];
        (name, value.as_ref())
    }
}
