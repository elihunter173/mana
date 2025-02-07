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
    // primitives
    Unit,
    Bool,
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    // structural row type
    Object(RowTy),
    // complex types
    NewType(UniqId, Box<Type>),
    // linear NewType
    Resource(UniqId, Box<Type>),
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
    columns: BTreeMap<String, Type>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct UniqId(u64);

#[derive(Debug, PartialEq, Eq, Hash)]
struct FnTy {
    param: Box<Type>,
    rtn: Box<Type>,
}

// TODO: This is almost certainly a bad representation
#[derive(Default, Debug, PartialEq, Eq, Hash)]
struct UnionTy {
    options: Vec<Type>,
}
