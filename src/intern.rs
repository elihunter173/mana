use lasso::{Key, Rodeo, Spur};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(Spur);

unsafe impl Key for Symbol {
    fn into_usize(self) -> usize {
        self.0.into_usize()
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        Spur::try_from_usize(int).map(Self)
    }
}

pub struct SymbolInterner {
    inner: Rodeo<Symbol>,
}

impl SymbolInterner {
    pub fn new() -> Self {
        Self { inner: Rodeo::new() }
    }

    // pub fn get(&self, val: &str) -> Option<Symbol> {
    //     self.inner.get(val)
    // }

    pub fn get_or_intern(&mut self, val: &str) -> Symbol {
        self.inner.get_or_intern(val)
    }

    pub fn resolve(&self, sym: &Symbol) -> &str {
        self.inner.resolve(sym)
    }
}
