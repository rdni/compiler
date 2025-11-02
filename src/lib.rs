//! Compiler library crate.
//!
//! This is the main library for a compiler that transforms source code
//! into LLVM IR and eventually executable binaries. The compilation
//! pipeline consists of:
//!
//! 1. Lexical analysis (tokenization)
//! 2. Parsing (AST construction)
//! 3. Type checking (semantic analysis)
//! 4. Code generation (LLVM IR)
//!
//! The library provides modules for each stage along with error handling
//! utilities and helper functions for displaying compilation errors.

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

/// Represents a position in source code.
///
/// Contains the character offset and the file name.
#[derive(Debug, Clone)]
pub struct Position(pub u32, pub Rc<String>);

impl Position {
    /// Creates a null position for synthetic nodes.
    pub fn null() -> Self {
        Position(0, Rc::new(String::from("<null>")))
    }
}

/// Represents a span (range) in source code.
///
/// Contains start and end positions to identify a region of source code.
#[derive(Debug, Clone)]
pub struct Span {
    /// The starting position of this span
    pub start: Position,
    /// The ending position of this span
    pub end: Position,
}

/// Retrieves the line of text at a given position in a file.
///
/// # Arguments
///
/// * `file` - Path to the source file
/// * `position` - Character offset in the file
///
/// # Returns
///
/// A tuple containing:
/// - Line number (1-indexed)
/// - The line text
/// - Position within the line (0-indexed)
///
/// # Panics
///
/// Panics if the position exceeds the file length or the line cannot be found.
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
    use super::*;

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

    #[test]
    fn test_remove_starting_whitespace() {
        let (result, count) = remove_starting_whitespace("    hello");
        assert_eq!(result, "hello");
        assert_eq!(count, 4);
    }

    #[test]
    fn test_remove_starting_whitespace_no_leading_space() {
        let (result, count) = remove_starting_whitespace("hello");
        assert_eq!(result, "hello");
        assert_eq!(count, 0);
    }

    #[test]
    fn test_remove_starting_whitespace_only_spaces() {
        let (result, count) = remove_starting_whitespace("    ");
        assert_eq!(result, "");
        assert_eq!(count, 4);
    }

    #[test]
    fn test_remove_starting_whitespace_mixed() {
        let (result, count) = remove_starting_whitespace("  hello world");
        assert_eq!(result, "hello world");
        assert_eq!(count, 2);
    }

    #[test]
    fn test_position_creation() {
        let pos = Position(42, Rc::new("test.lang".to_string()));
        assert_eq!(pos.0, 42);
    }

    #[test]
    fn test_position_null() {
        let pos = Position::null();
        assert_eq!(pos.0, 0);
        assert_eq!(*pos.1, "<null>");
    }

    #[test]
    fn test_span_creation() {
        let start = Position(10, Rc::new("test.lang".to_string()));
        let end = Position(20, Rc::new("test.lang".to_string()));
        let span = Span {
            start: start.clone(),
            end: end.clone(),
        };
        
        assert_eq!(span.start.0, 10);
        assert_eq!(span.end.0, 20);
    }
}

/// Displays a formatted compilation error to stdout.
///
/// Shows the error message, file location, line number, source line,
/// and a pointer to the error position.
///
/// # Arguments
///
/// * `error` - The error to display
/// * `file` - Path to the source file
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

/// Helper function to remove leading whitespace from a string.
///
/// # Arguments
///
/// * `string` - The string to process
///
/// # Returns
///
/// A tuple containing:
/// - The string with leading whitespace removed
/// - The number of whitespace characters removed
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
