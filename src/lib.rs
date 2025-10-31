#![allow(clippy::module_inception)]

use std::{fs, path::PathBuf, rc::Rc};

use crate::errors::errors::{Error, ErrorTip};

pub mod ast;
pub mod compiler;
pub mod errors;
pub mod lexer;
pub mod macros;
pub mod parser;
pub mod type_checker;

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
    pub end: Position,
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
        let (line_number, line, line_pos) =
            super::get_line_at_position(std::path::PathBuf::from("tests/test_file.txt"), 10);
        assert_eq!(line_number, 1);
        assert_eq!(line, "Hello, world!\n");
        assert_eq!(line_pos, 10);

        let (line_number, line, line_pos) =
            super::get_line_at_position(std::path::PathBuf::from("tests/test_file.txt"), 34);
        assert_eq!(line_number, 4);
        assert_eq!(line, "Testing { }\n");
        assert_eq!(line_pos, 8);
    }
}

pub fn display_error(error: Error, file: PathBuf) {
    /*
        error: message
        -> final.lang
           |
        20 | let a = #;
           | --------^
    */

    let position = error.get_position();
    let (line, line_text, line_pos) = get_line_at_position(file.clone(), position.0);

    let line_string = line.to_string();
    let padding = line_string.len() + 2;

    if let ErrorTip::None = error.get_tip() {
        println!("Error: {}", error.get_error_name());
    } else {
        println!("Error: {} ({})", error.get_error_name(), error.get_tip());
    }
    println!("-> {}", file.as_os_str().to_string_lossy());
    println!("{:>padding$}", "|");

    let (line_text_removed, removed_whitespace) = remove_starting_whitespace(&line_text);
    println!("{} | {}", line_string, line_text_removed.trim());

    let arrows = line_pos - removed_whitespace + 1;

    println!("{:>padding$} {:->arrows$}", "|", "^");
}

fn remove_starting_whitespace(string: &str) -> (String, usize) {
    let mut start = 0;
    for c in string.chars() {
        if c == ' ' {
            start += 1;
        } else {
            break;
        }
    }

    (String::from(&string[start..]), start)
}
