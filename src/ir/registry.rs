use std::collections::BTreeMap;

use crate::{
    ast::Ident,
    ir::{Expr, Variable},
    ty::{FloatTy, IntTy, RowTy, TyKind, Type, UIntTy},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DefId(usize);

const PRIMITIVES: [Type; 5] = [
    Type {
        kind: TyKind::Object(RowTy { columns: BTreeMap::new() }),
    },
    Type { kind: TyKind::Bool },
    Type { kind: TyKind::Int(IntTy::I32) },
    Type { kind: TyKind::UInt(UIntTy::U32) },
    Type { kind: TyKind::Float(FloatTy::F64) },
];
const UNIT_TYPE_ID: TypeId = TypeId(0);
const BOOL_TYPE_ID: TypeId = TypeId(1);
const INT_TYPE_ID: TypeId = TypeId(2);
const UINT_TYPE_ID: TypeId = TypeId(3);
const FLOAT_TYPE_ID: TypeId = TypeId(4);

#[derive(Debug)]
pub struct Registry {
    types: Vec<Type>,
    variables: Vec<Variable>,
    defs: Vec<(Ident, Option<Expr>)>,
}

impl Registry {
    pub fn with_primitives() -> Self {
        Self {
            types: Vec::from(PRIMITIVES),
            variables: Vec::new(),
            defs: Vec::new(),
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
