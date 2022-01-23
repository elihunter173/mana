//! This module is shamelessly "inspired" by Rust's ty module

use std::collections::{BTreeMap, HashMap};

use crate::intern::{Symbol, SymbolInterner};

pub type Ty<'ctx> = &'ctx TyS;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ManaPath {
    // TODO: This must be non-empty
    pub idents: Vec<Symbol>,
}

#[derive(Debug)]
pub struct TyResolver {
    map: HashMap<ManaPath, TyS>,
}

impl TyResolver {
    pub fn with_primitives(interner: &mut SymbolInterner) -> Self {
        let mut path = |s: &str| ManaPath {
            idents: Vec::from([interner.get_or_intern(s)]),
        };

        Self {
            map: HashMap::from([
                (path("Bool"), TyS { kind: TyKind::Bool }),
                (path("Int"), TyS { kind: TyKind::Int(DEFAULT_INT) }),
                (path("Int8"), TyS { kind: TyKind::Int(IntTy::I8) }),
                (path("Int16"), TyS { kind: TyKind::Int(IntTy::I16) }),
                (path("Int32"), TyS { kind: TyKind::Int(IntTy::I32) }),
                (path("Int64"), TyS { kind: TyKind::Int(IntTy::I64) }),
                (path("ISize"), TyS { kind: TyKind::Int(IntTy::ISize) }),
                (path("UInt"), TyS { kind: TyKind::UInt(DEFAULT_UINT) }),
                (path("UInt8"), TyS { kind: TyKind::UInt(UIntTy::U8) }),
                (path("UInt16"), TyS { kind: TyKind::UInt(UIntTy::U16) }),
                (path("UInt32"), TyS { kind: TyKind::UInt(UIntTy::U32) }),
                (path("UInt64"), TyS { kind: TyKind::UInt(UIntTy::U64) }),
                (path("USize"), TyS { kind: TyKind::UInt(UIntTy::USize) }),
                (path("Float32"), TyS { kind: TyKind::Float(FloatTy::F32) }),
                (path("Float64"), TyS { kind: TyKind::Float(FloatTy::F64) }),
                (path("String"), TyS { kind: TyKind::String }),
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

    pub fn resolve(&self, path: &ManaPath) -> Option<Ty<'_>> {
        self.map.get(path)
    }

    pub fn define(&mut self, path: &ManaPath, ty: TyS) -> Result<(), ()> {
        match self.map.try_insert(path.clone(), ty) {
            Ok(_) => Ok(()),
            Err(_) => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TyKind {
    Bool,
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    String,
    // TODO: This should be an empty tuple sometime
    Unit,
    Tuple(Vec<TyS>),
    // Key must be unique
    Struct(BTreeMap<String, TyS>),
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
