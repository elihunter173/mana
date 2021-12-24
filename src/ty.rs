//! This module is shamelessly "inspired" by Rust's ty module

use std::collections::{BTreeMap, HashMap};

pub type Ty<'ctx> = &'ctx TyS;

#[derive(Debug)]
pub struct TyInterner {
    map: HashMap<String, TyS>,
}

impl TyInterner {
    pub fn with_primitives() -> Self {
        Self {
            map: HashMap::from([
                ("Bool".to_owned(), TyS { kind: TyKind::Bool }),
                ("Int".to_owned(), TyS { kind: TyKind::Int(IntTy::I32) }),
                ("Int8".to_owned(), TyS { kind: TyKind::Int(IntTy::I8) }),
                ("Int16".to_owned(), TyS { kind: TyKind::Int(IntTy::I16) }),
                ("Int32".to_owned(), TyS { kind: TyKind::Int(IntTy::I32) }),
                ("Int64".to_owned(), TyS { kind: TyKind::Int(IntTy::I64) }),
                ("ISize".to_owned(), TyS { kind: TyKind::Int(IntTy::ISize) }),
                ("UInt".to_owned(), TyS { kind: TyKind::UInt(UIntTy::U32) }),
                ("UInt8".to_owned(), TyS { kind: TyKind::UInt(UIntTy::U8) }),
                ("UInt16".to_owned(), TyS { kind: TyKind::UInt(UIntTy::U16) }),
                ("UInt32".to_owned(), TyS { kind: TyKind::UInt(UIntTy::U32) }),
                ("UInt64".to_owned(), TyS { kind: TyKind::UInt(UIntTy::U64) }),
                (
                    "USize".to_owned(),
                    TyS { kind: TyKind::UInt(UIntTy::USize) },
                ),
                (
                    "Float32".to_owned(),
                    TyS { kind: TyKind::Float(FloatTy::F32) },
                ),
                (
                    "Float64".to_owned(),
                    TyS { kind: TyKind::Float(FloatTy::F64) },
                ),
                ("String".to_owned(), TyS { kind: TyKind::String }),
            ]),
        }
    }

    pub fn unit(&self) -> Ty<'_> {
        &TyS { kind: TyKind::Unit }
    }

    pub fn bool(&self) -> Ty<'_> {
        &TyS { kind: TyKind::Bool }
    }

    pub fn resolve(&self, path: &str) -> Option<Ty<'_>> {
        self.map.get(path)
    }

    pub fn define(&mut self, path: &str, ty: TyS) -> Result<(), ()> {
        match self.map.try_insert(path.to_owned(), ty) {
            Ok(_) => Ok(()),
            Err(_) => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TyS {
    pub kind: TyKind,
}

impl TyS {
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

#[derive(Debug, PartialEq, Eq)]
pub enum TyKind {
    Bool,
    // TODO: This should definitely be a tuple, but I haven't implemented tuples yet
    Unit,
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    String,
    Tuple(Vec<TyS>),
    // Key must be unique
    Struct(BTreeMap<String, TyS>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum IntTy {
    ISize,
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum UIntTy {
    USize,
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum FloatTy {
    F32,
    F64,
}
