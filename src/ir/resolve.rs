use std::collections::{hash_map, HashMap};

use crate::{
    intern::{Symbol, SymbolInterner},
    ty::{FloatTy, IntTy, TyKind, Type, UIntTy},
};

use super::registry::{Registry, TypeId, VariableId};

// TODO: Unify FunctionId and VariableIds
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ObjectId {
    Type(TypeId),
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

    pub fn as_variable_id(self) -> Option<VariableId> {
        if let Self::Variable(id) = self {
            Some(id)
        } else {
            None
        }
    }
}

#[derive(Debug)]
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

        let mut prelude = HashMap::new();

        // primitives
        for (sym, ty_id) in [
            (sym("Bool"), registry.bool()),
            (sym("Int"), registry.int()),
            (sym("UInt"), registry.uint()),
            (sym("Float"), registry.float()),
        ] {
            prelude.insert(sym, ObjectId::Type(ty_id));
        }

        // prelude
        for (sym, ty_kind) in [
            // signed int
            (sym("Int8"), TyKind::Int(IntTy::I8)),
            (sym("Int16"), TyKind::Int(IntTy::I16)),
            (sym("Int32"), TyKind::Int(IntTy::I32)),
            (sym("Int64"), TyKind::Int(IntTy::I64)),
            (sym("ISize"), TyKind::Int(IntTy::ISize)),
            // unsigned int
            (sym("UInt8"), TyKind::UInt(UIntTy::U8)),
            (sym("UInt16"), TyKind::UInt(UIntTy::U16)),
            (sym("UInt32"), TyKind::UInt(UIntTy::U32)),
            (sym("UInt64"), TyKind::UInt(UIntTy::U64)),
            (sym("USize"), TyKind::UInt(UIntTy::USize)),
            // floats
            (sym("Float32"), TyKind::Float(FloatTy::F32)),
            (sym("Float64"), TyKind::Float(FloatTy::F64)),
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
        match current_scope.entry(sym) {
            hash_map::Entry::Vacant(slot) => {
                slot.insert(obj);
                Ok(())
            }
            hash_map::Entry::Occupied(_) => Err(ResolverError::DuplicateItem),
        }
    }

    pub fn define_type(&mut self, sym: Symbol, id: TypeId) -> Result<(), ResolverError> {
        self.define(sym, ObjectId::Type(id))
    }

    pub fn define_variable(&mut self, sym: Symbol, id: VariableId) -> Result<(), ResolverError> {
        self.define(sym, ObjectId::Variable(id))
    }
}
