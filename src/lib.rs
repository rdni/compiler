use std::rc::Rc;

pub mod lexer;
pub mod macros;
pub mod ast;
pub mod parser;
pub mod errors;
pub mod type_checker;
pub mod compiler;

extern crate regex;

#[derive(Debug, Clone)]
pub struct Position(pub u32, pub Rc<String>);

impl Position {
    pub fn null() -> Self {
        Position(0, Rc::new(String::from("<null>")))
    }
}

#[derive(Debug, Clone)]
pub struct Span {
    pub start: Position,
    pub end: Position
}