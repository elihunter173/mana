//! This `ty.rs` is shamelessly "inspired" by Rust's ty module

pub struct TyS {
    kind: TyKind,
}

pub enum TyKind {
    Bool,
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    String,
}

// TODO: Add U/I128?

pub enum IntTy {
    ISize,
    I8,
    I16,
    I32,
    I64,
}

pub enum UIntTy {
    USize,
    U8,
    U16,
    U32,
    U64,
}

pub enum FloatTy {
    F32,
    F64,
}
