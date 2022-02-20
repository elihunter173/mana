use std::collections::BTreeMap;

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
    Bool,
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    String,
    // TODO: This should be an empty tuple sometime
    Unit,
    Tuple(Vec<Type>),
    // Key must be unique
    Struct(BTreeMap<String, Type>),
}

pub const DEFAULT_INT: IntTy = IntTy::I32;
pub const DEFAULT_UINT: UIntTy = UIntTy::U32;
pub const DEFAULT_FLOAT: FloatTy = FloatTy::F64;

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
