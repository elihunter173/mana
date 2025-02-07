use crate::{
    ir::{FunctionBody, FunctionSignature, Variable},
    ty::Type,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

#[derive(Debug)]
pub struct Registry {
    types: Vec<Type>,
    functions: Vec<(FunctionSignature, Option<FunctionBody>)>,
    variables: Vec<Variable>,
}

impl Registry {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            functions: Vec::new(),
            variables: Vec::new(),
        }
    }

    pub fn get_type(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    pub fn get_function(&self, id: FunctionId) -> (&FunctionSignature, Option<&FunctionBody>) {
        let (sig, body) = &self.functions[id.0];
        (sig, body.as_ref())
    }

    pub fn get_variable(&self, id: VariableId) -> &Variable {
        &self.variables[id.0]
    }

    pub fn register_type(&mut self, ty: Type) -> TypeId {
        self.types.push(ty);
        TypeId(self.types.len() - 1)
    }

    pub fn declare_function(&mut self, sig: FunctionSignature) -> FunctionId {
        self.functions.push((sig, None));
        FunctionId(self.functions.len() - 1)
    }

    pub fn define_function(&mut self, func: FunctionId, body: FunctionBody) {
        self.functions[func.0].1 = Some(body);
    }

    pub fn register_variable(&mut self, var: Variable) -> VariableId {
        self.variables.push(var);
        VariableId(self.variables.len() - 1)
    }
}
