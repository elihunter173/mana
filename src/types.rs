use core::fmt::{self, Formatter};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(&'static str),
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
