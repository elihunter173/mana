//! This `ty.rs` is shamelessly "inspired" by Rust's ty module

#[derive(PartialEq, Eq, Hash)]
pub struct TyS {
    kind: TyKind,
}

#[derive(PartialEq, Eq, Hash)]
pub enum TyKind {
    Bool,
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    String,
    Tuple(Vec<TyS>),
}

#[derive(PartialEq, Eq, Hash)]
pub enum IntTy {
    ISize,
    I8,
    I16,
    I32,
    I64,
}

#[derive(PartialEq, Eq, Hash)]
pub enum UIntTy {
    USize,
    U8,
    U16,
    U32,
    U64,
}

#[derive(PartialEq, Eq, Hash)]
pub enum FloatTy {
    F32,
    F64,
}
