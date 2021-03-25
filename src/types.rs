use core::fmt::{self, Formatter};

#[derive(Debug, PartialEq)]
pub enum ManaliType {
    Bool(bool),
    Number(f64),
    String(String),
}

impl fmt::Display for ManaliType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ManaliType::Bool(val) => write!(f, "{:?}", val),
            ManaliType::Number(val) => write!(f, "{:?}", val),
            ManaliType::String(val) => write!(f, "{:?}", val),
        }
    }
}

impl From<bool> for ManaliType {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<f64> for ManaliType {
    fn from(v: f64) -> Self {
        Self::Number(v)
    }
}

impl From<&str> for ManaliType {
    fn from(v: &str) -> Self {
        Self::String(v.to_owned())
    }
}
