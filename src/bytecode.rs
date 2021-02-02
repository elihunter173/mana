use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum OpCode {
    // Unary
    LoadImd,
    // Binary
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    // Special
    Print,
    Typeof,
}
