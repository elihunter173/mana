use std::collections::HashMap;

use crate::{
    intern::{Symbol, SymbolInterner},
    ty::{FloatTy, IntTy, Ty, TyKind, TyS, UIntTy, DEFAULT_FLOAT, DEFAULT_INT, DEFAULT_UINT},
};

// TODO: This must be non-empty
// TODO: Make this a new type
pub type ManaPath = Vec<Symbol>;

#[derive(Debug)]
pub enum ManaObject {
    Type(TyS),
}

#[derive(Debug)]
pub struct Resolver {
    map: HashMap<ManaPath, ManaObject>,
}

impl Resolver {
    pub fn with_primitives(interner: &mut SymbolInterner) -> Self {
        let mut path = |s: &str| Vec::from([interner.get_or_intern(s)]);
        let ty = |kind| ManaObject::Type(TyS { kind });

        Self {
            map: HashMap::from([
                (path("Bool"), ty(TyKind::Bool)),
                (path("Int"), ty(TyKind::Int(DEFAULT_INT))),
                (path("Int8"), ty(TyKind::Int(IntTy::I8))),
                (path("Int16"), ty(TyKind::Int(IntTy::I16))),
                (path("Int32"), ty(TyKind::Int(IntTy::I32))),
                (path("Int64"), ty(TyKind::Int(IntTy::I64))),
                (path("ISize"), ty(TyKind::Int(IntTy::ISize))),
                (path("UInt"), ty(TyKind::UInt(DEFAULT_UINT))),
                (path("UInt8"), ty(TyKind::UInt(UIntTy::U8))),
                (path("UInt16"), ty(TyKind::UInt(UIntTy::U16))),
                (path("UInt32"), ty(TyKind::UInt(UIntTy::U32))),
                (path("UInt64"), ty(TyKind::UInt(UIntTy::U64))),
                (path("USize"), ty(TyKind::UInt(UIntTy::USize))),
                (path("Float32"), ty(TyKind::Float(FloatTy::F32))),
                (path("Float64"), ty(TyKind::Float(FloatTy::F64))),
                (path("String"), ty(TyKind::String)),
            ]),
        }
    }

    pub fn bool(&self) -> Ty<'_> {
        &TyS { kind: TyKind::Bool }
    }

    pub fn unit(&self) -> Ty<'_> {
        &TyS { kind: TyKind::Unit }
    }

    pub fn int(&self) -> Ty<'_> {
        &TyS { kind: TyKind::Int(DEFAULT_INT) }
    }

    pub fn uint(&self) -> Ty<'_> {
        &TyS { kind: TyKind::UInt(DEFAULT_UINT) }
    }

    pub fn float(&self) -> Ty<'_> {
        &TyS { kind: TyKind::Float(DEFAULT_FLOAT) }
    }

    pub fn string(&self) -> Ty<'_> {
        &TyS { kind: TyKind::String }
    }

    pub fn resolve(&self, scope: &ManaPath, path: &ManaPath) -> Option<&ManaObject> {
        // TODO: Use ManaPath
        self.map.get(path)
    }

    pub fn define(&mut self, path: &ManaPath, ty: TyS) -> Result<(), ()> {
        match self.map.try_insert(path.clone(), ManaObject::Type(ty)) {
            Ok(_) => Ok(()),
            Err(_) => Err(()),
        }
    }
}
