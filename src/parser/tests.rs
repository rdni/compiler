//! Unit tests for the parser module.
//!
//! This module contains tests for parsing various language constructs including:
//! - Variable declarations
//! - Function declarations
//! - Expressions
//! - Control flow statements
//! - Struct definitions

use crate::lexer::lexer::tokenize;
use super::parser::parse;

#[test]
fn test_parse_variable_declaration() {
    let source = "let x = 42;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_const_declaration() {
    let source = "const PI = 3.14;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_function_declaration() {
    let source = "fn add(a: i32, b: i32) -> i32 { return a + b; }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_if_statement() {
    let source = "if x > 0 { print(\"positive\"); }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_if_else_statement() {
    let source = "if x > 0 { print(\"positive\"); } else { print(\"negative\"); }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_while_loop() {
    let source = "while x < 10 { x = x + 1; }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_struct_definition() {
    let source = "struct Point { x: i32, y: i32 }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_struct_instantiation() {
    let source = "let p = new Point { x: 10, y: 20 };".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_binary_expression() {
    let source = "let result = 5 + 3 * 2;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_comparison_expression() {
    let source = "let is_equal = x == y;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_logical_expression() {
    let source = "let result = x > 0 && y < 10;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_function_call() {
    let source = "print(\"Hello, World!\");".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_member_access() {
    let source = "let x_coord = point.x;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_assignment() {
    let source = "x = 42;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_compound_assignment() {
    let source = "x += 5;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_return_statement() {
    let source = "return 42;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_parenthesized_expression() {
    let source = "let result = (5 + 3) * 2;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_unary_expression() {
    let source = "let neg = -x;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_multiple_statements() {
    let source = "let x = 10; let y = 20; let z = x + y;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_nested_blocks() {
    let source = "{ let x = 10; { let y = 20; } }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_string_literal() {
    let source = r#"let msg = "Hello";"#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_numeric_literal() {
    let source = "let pi = 3.14;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_empty_program() {
    let source = "".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    assert!(result.is_ok());
}

#[test]
fn test_parse_syntax_error_missing_semicolon() {
    let source = "let x = 42".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    // Should fail due to missing semicolon
    assert!(result.is_err());
}

#[test]
fn test_parse_syntax_error_unexpected_token() {
    let source = "let = 42;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    
    // Should fail due to missing identifier
    assert!(result.is_err());
}
