use std::collections::BTreeMap;

use crate::ir::registry::TypeId;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TyKind,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        match self.kind {
            TyKind::Int(_) | TyKind::UInt(_) | TyKind::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        self.kind == TyKind::Bool
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TyKind {
    // primitives
    Unit,
    Bool,
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    // structural row type
    Object(RowTy),
    // complex types
    NewType(UniqId, TypeId),
    // linear NewType
    Resource(UniqId, TypeId),
    Fn(FnTy),
    Union(UnionTy),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum IntTy {
    ISize,
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum UIntTy {
    USize,
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct RowTy {
    pub columns: BTreeMap<String, TypeId>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct UniqId(u64);

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FnTy {
    pub params: Vec<TypeId>,
    pub rtn: TypeId,
}

#[derive(Default, Debug, PartialEq, Eq, Hash)]
pub struct UnionTy {
    options: Vec<TypeId>,
}
