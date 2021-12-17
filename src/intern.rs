use std::collections::HashSet;

use crate::ty::TyS;

// TODO: Improve this
pub struct TyInterner {
    data: HashSet<TyS>,
}

impl TyInterner {
    pub fn new() -> Self {
        Self { data: Default::default() }
    }

    pub fn intern(&mut self, tys: TyS) -> &TyS {
        self.data.get_or_insert(tys)
    }
}
