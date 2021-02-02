use std::{
    convert::TryFrom,
    io::{Cursor, ErrorKind},
};

use byteorder::ReadBytesExt;

use crate::{bytecode::OpCode, types::Value};

#[derive(Debug)]
pub struct Interpreter<'buf> {
    code: Cursor<&'buf [u8]>,
    immediates: Vec<Value>,
    stack: Vec<Value>,
}

impl<'buf> Interpreter<'buf> {
    pub fn new(code: &'buf [u8], immediates: Vec<Value>) -> Self {
        Self {
            code: Cursor::new(code),
            immediates,
            stack: Vec::new(),
        }
    }

    fn step(&mut self) -> Result<(), ()> {
        let op = match self.code.read_u8() {
            Ok(byte) => OpCode::try_from(byte).unwrap(),
            Err(err) if err.kind() == ErrorKind::UnexpectedEof => return Err(()),
            Err(err) => panic!(err),
        };
        use OpCode::*;
        match op {
            Mul | Div | Sub | Add | Eq | Neq => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                match (op, left, right) {
                    (Mul, Value::Number(left), Value::Number(right)) => {
                        self.stack.push(Value::Number(left * right));
                    }
                    (Div, Value::Number(left), Value::Number(right)) => {
                        self.stack.push(Value::Number(left / right));
                    }
                    (Add, Value::Number(left), Value::Number(right)) => {
                        self.stack.push(Value::Number(left + right));
                    }
                    (Sub, Value::Number(left), Value::Number(right)) => {
                        self.stack.push(Value::Number(left - right));
                    }
                    (Eq, left, right) => {
                        self.stack.push(Value::Bool(left == right));
                    }
                    (Neq, left, right) => {
                        self.stack.push(Value::Bool(left != right));
                    }
                    _ => panic!("type error"),
                }
            }
            LoadImd => {
                let id = self.code.read_u8().unwrap();
                self.stack.push(*self.immediates.get(id as usize).unwrap());
            }
            Print => {
                println!("{}", self.stack.pop().unwrap());
            }
            Typeof => {
                let val = self.stack.pop().unwrap();
                let ty = match val {
                    Value::Bool(_) => "bool",
                    Value::Number(_) => "number",
                    Value::String(_) => "string",
                };
                self.stack.push(Value::String(ty));
            }
        }
        Ok(())
    }

    pub fn run(&mut self) {
        while let Ok(()) = self.step() {}
    }
}
