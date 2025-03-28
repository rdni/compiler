#![allow(clippy::module_inception)]

use std::{fs, path::PathBuf, rc::Rc};

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

pub fn get_line_at_position(file: PathBuf, position: u32) -> (usize, String, usize) {
    let content = fs::read_to_string(&file).unwrap();
    let pos = position as usize;

    if pos >= content.len() {
        panic!("Position exceeds file length");
    }

    let mut start = 0;
    let mut line_number = 1;

    for line in content.split_inclusive('\n') {
        let end = start + line.len();
        
        if (start..end).contains(&pos) {
            let line_pos = pos - start;
            return (line_number, line.to_string(), line_pos);
        }
        
        start = end;
        line_number += 1;
    }

    panic!("Failed to find line containing position");
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_get_line_at_position() {
        let (line_number, line, line_pos) = super::get_line_at_position(std::path::PathBuf::from("tests/test_file.txt"), 10);
        assert_eq!(line_number, 1);
        assert_eq!(line, "Hello, world!\n");
        assert_eq!(line_pos, 10);

        let (line_number, line, line_pos) = super::get_line_at_position(std::path::PathBuf::from("tests/test_file.txt"), 34);
        assert_eq!(line_number, 4);
        assert_eq!(line, "Testing { }\n");
        assert_eq!(line_pos, 8);
    }
}