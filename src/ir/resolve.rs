use std::collections::HashMap;

use crate::{
    intern::{Symbol, SymbolInterner},
    ty::{FloatTy, IntTy, TyKind, Type, UIntTy},
};

use super::registry::{FunctionId, Registry, TypeId, VariableId};

// TODO: I could do a smarter representation of these Ids (u32 with high bits as discriminant?)
// TODO: Unify FunctionId and VariableIds
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ObjectId {
    Type(TypeId),
    Function(FunctionId),
    Variable(VariableId),
}

impl ObjectId {
    pub fn as_type_id(self) -> Option<TypeId> {
        if let Self::Type(id) = self {
            Some(id)
        } else {
            None
        }
    }

    pub fn as_function_id(self) -> Option<FunctionId> {
        if let Self::Function(id) = self {
            Some(id)
        } else {
            None
        }
    }

    pub fn as_variable_id(self) -> Option<VariableId> {
        if let Self::Variable(id) = self {
            Some(id)
        } else {
            None
        }
    }
}

pub enum ResolverError {
    DuplicateItem,
}

#[derive(Debug)]
pub struct Resolver {
    // TODO: Use non-empty?
    scopes: Vec<HashMap<Symbol, ObjectId>>,
}

impl Resolver {
    // TODO: Make it clear that resolver doesn't save these values. Probably with a new and then an
    // init
    pub fn with_prelude(interner: &mut SymbolInterner, registry: &mut Registry) -> Self {
        let mut sym = |s: &str| interner.get_or_intern(s);

        let mut prelude = HashMap::from([
            // Basic types
            (sym("Bool"), ObjectId::Type(registry.bool())),
            (sym("Int"), ObjectId::Type(registry.int())),
            (sym("UInt"), ObjectId::Type(registry.uint())),
            (sym("Float"), ObjectId::Type(registry.float())),
            (sym("String"), ObjectId::Type(registry.string())),
        ]);

        // Non-basic types still in prelude
        for (sym, ty_kind) in [
            (sym("Int8"), TyKind::Int(IntTy::I8)),
            (sym("Int16"), TyKind::Int(IntTy::I16)),
            (sym("Int32"), TyKind::Int(IntTy::I32)),
            (sym("Int64"), TyKind::Int(IntTy::I64)),
            (sym("ISize"), TyKind::Int(IntTy::ISize)),
            (sym("UInt8"), TyKind::UInt(UIntTy::U8)),
            (sym("UInt16"), TyKind::UInt(UIntTy::U16)),
            (sym("UInt32"), TyKind::UInt(UIntTy::U32)),
            (sym("UInt64"), TyKind::UInt(UIntTy::U64)),
            (sym("USize"), TyKind::UInt(UIntTy::USize)),
            (sym("Float32"), TyKind::Float(FloatTy::F32)),
        ] {
            let type_id = registry.register_type(Type { kind: ty_kind });
            prelude.insert(sym, ObjectId::Type(type_id));
        }

        Self { scopes: Vec::from([prelude]) }
    }

    // TODO: Think about having a with_scope rather than enter and exit scope... Better with error
    // handling probably
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop().expect("cannot remove last scope");
    }

    pub fn resolve(&self, sym: &Symbol) -> Option<ObjectId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.get(sym) {
                return Some(id);
            }
        }
        None
    }

    fn define(&mut self, sym: Symbol, obj: ObjectId) -> Result<(), ResolverError> {
        let current_scope = self.scopes.last_mut().expect("must have at least 1 scope");
        match current_scope.try_insert(sym, obj) {
            Ok(_) => Ok(()),
            Err(_) => Err(ResolverError::DuplicateItem),
        }
    }

    pub fn define_type(&mut self, sym: Symbol, id: TypeId) -> Result<(), ResolverError> {
        self.define(sym, ObjectId::Type(id))
    }

    pub fn define_function(&mut self, sym: Symbol, id: FunctionId) -> Result<(), ResolverError> {
        self.define(sym, ObjectId::Function(id))
    }

    pub fn define_variable(&mut self, sym: Symbol, id: VariableId) -> Result<(), ResolverError> {
        self.define(sym, ObjectId::Variable(id))
    }
}
