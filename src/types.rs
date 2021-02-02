use core::fmt::{self, Formatter};

#[derive(Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(val) => write!(f, "{:?}", val),
            Value::Number(val) => write!(f, "{:?}", val),
            Value::String(val) => write!(f, "{:?}", val),
        }
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Number(v)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self::String(v.to_owned())
    }
}
