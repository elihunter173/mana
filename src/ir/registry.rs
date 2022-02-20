use crate::{
    ir::{Function, Variable},
    ty::{FloatTy, TyKind, Type, DEFAULT_INT, DEFAULT_UINT},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

const BASIC_TYPES: [Type; 6] = [
    Type { kind: TyKind::Tuple(Vec::new()) },
    Type { kind: TyKind::Bool },
    Type { kind: TyKind::Int(DEFAULT_INT) },
    Type { kind: TyKind::UInt(DEFAULT_UINT) },
    Type { kind: TyKind::Float(FloatTy::F64) },
    Type { kind: TyKind::String },
];
const UNIT_TYPE_ID: TypeId = TypeId(0);
const BOOL_TYPE_ID: TypeId = TypeId(1);
const INT_TYPE_ID: TypeId = TypeId(2);
const UINT_TYPE_ID: TypeId = TypeId(3);
const FLOAT_TYPE_ID: TypeId = TypeId(4);
const STRING_TYPE_ID: TypeId = TypeId(5);

#[derive(Debug)]
pub struct Registry {
    types: Vec<Type>,
    functions: Vec<Function>,
    variables: Vec<Variable>,
}

impl Registry {
    pub fn with_basic_types() -> Self {
        Self {
            types: Vec::from(BASIC_TYPES),
            functions: Vec::new(),
            variables: Vec::new(),
        }
    }

    pub fn unit(&self) -> TypeId {
        UNIT_TYPE_ID
    }

    pub fn bool(&self) -> TypeId {
        BOOL_TYPE_ID
    }

    pub fn int(&self) -> TypeId {
        INT_TYPE_ID
    }

    pub fn uint(&self) -> TypeId {
        UINT_TYPE_ID
    }

    pub fn float(&self) -> TypeId {
        FLOAT_TYPE_ID
    }

    pub fn string(&self) -> TypeId {
        STRING_TYPE_ID
    }

    pub fn get_type(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    pub fn get_function(&self, id: FunctionId) -> &Function {
        &self.functions[id.0]
    }

    pub fn get_variable(&self, id: VariableId) -> &Variable {
        &self.variables[id.0]
    }

    pub fn register_type(&mut self, ty: Type) -> TypeId {
        self.types.push(ty);
        TypeId(self.types.len() - 1)
    }

    pub fn register_function(&mut self, func: Function) -> FunctionId {
        self.functions.push(func);
        FunctionId(self.functions.len() - 1)
    }

    pub fn register_variable(&mut self, var: Variable) -> VariableId {
        self.variables.push(var);
        VariableId(self.variables.len() - 1)
    }
}
