//! Unit tests for the lexer module.
//!
//! This module contains comprehensive tests for tokenization including:
//! - Keywords and identifiers
//! - Numeric literals (integers and floats)
//! - String literals with escape sequences
//! - Operators and punctuation
//! - Comments
//! - Error cases

use super::{lexer::tokenize, tokens::TokenKind};

#[test]
fn test_tokenize_keywords() {
    let source = "let const fn if else while for return new import from export struct extern".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Const);
    assert_eq!(tokens[2].kind, TokenKind::Fn);
    assert_eq!(tokens[3].kind, TokenKind::If);
    assert_eq!(tokens[4].kind, TokenKind::Else);
    assert_eq!(tokens[5].kind, TokenKind::While);
    assert_eq!(tokens[6].kind, TokenKind::For);
    assert_eq!(tokens[7].kind, TokenKind::Return);
    assert_eq!(tokens[8].kind, TokenKind::New);
    assert_eq!(tokens[9].kind, TokenKind::Import);
    assert_eq!(tokens[10].kind, TokenKind::From);
    assert_eq!(tokens[11].kind, TokenKind::Export);
    assert_eq!(tokens[12].kind, TokenKind::Struct);
    assert_eq!(tokens[13].kind, TokenKind::Extern);
    assert_eq!(tokens[14].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_identifiers() {
    let source = "foo bar baz_123 _underscore CamelCase".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[0].value, "foo");
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].value, "bar");
    assert_eq!(tokens[2].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].value, "baz_123");
    assert_eq!(tokens[3].kind, TokenKind::Identifier);
    assert_eq!(tokens[3].value, "_underscore");
    assert_eq!(tokens[4].kind, TokenKind::Identifier);
    assert_eq!(tokens[4].value, "CamelCase");
    assert_eq!(tokens[5].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_numbers() {
    let source = "42 3.14 0 100.5".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Number);
    assert_eq!(tokens[0].value, "42");
    assert_eq!(tokens[1].kind, TokenKind::Number);
    assert_eq!(tokens[1].value, "3.14");
    assert_eq!(tokens[2].kind, TokenKind::Number);
    assert_eq!(tokens[2].value, "0");
    assert_eq!(tokens[3].kind, TokenKind::Number);
    assert_eq!(tokens[3].value, "100.5");
    assert_eq!(tokens[4].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_strings() {
    let source = r#""hello" "world" "multiple words""#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::String);
    assert_eq!(tokens[0].value, "hello");
    assert_eq!(tokens[1].kind, TokenKind::String);
    assert_eq!(tokens[1].value, "world");
    assert_eq!(tokens[2].kind, TokenKind::String);
    assert_eq!(tokens[2].value, "multiple words");
    assert_eq!(tokens[3].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_string_escapes() {
    let source = r#""hello\nworld" "tab\there" "backslash\\" "hex\x41""#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::String);
    assert_eq!(tokens[0].value, "hello\nworld");
    assert_eq!(tokens[1].kind, TokenKind::String);
    assert_eq!(tokens[1].value, "tab\there");
    assert_eq!(tokens[2].kind, TokenKind::String);
    assert_eq!(tokens[2].value, "backslash\\");
    assert_eq!(tokens[3].kind, TokenKind::String);
    assert_eq!(tokens[3].value, "hexA");
    assert_eq!(tokens[4].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_escaped_quote_in_string() {
    let source = r#""quote\"test""#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::String);
    assert_eq!(tokens[0].value, "quote\"test");
    assert_eq!(tokens[1].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_operators() {
    let source = "+ - * / % == != < > <= >= = && ||".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Plus);
    assert_eq!(tokens[1].kind, TokenKind::Dash);
    assert_eq!(tokens[2].kind, TokenKind::Star);
    assert_eq!(tokens[3].kind, TokenKind::Slash);
    assert_eq!(tokens[4].kind, TokenKind::Percent);
    assert_eq!(tokens[5].kind, TokenKind::Equals);
    assert_eq!(tokens[6].kind, TokenKind::NotEquals);
    assert_eq!(tokens[7].kind, TokenKind::Less);  // Changed from LessThan
    assert_eq!(tokens[8].kind, TokenKind::Greater);
    assert_eq!(tokens[9].kind, TokenKind::LessEquals);
    assert_eq!(tokens[10].kind, TokenKind::GreaterEquals);
    assert_eq!(tokens[11].kind, TokenKind::Assignment);
    assert_eq!(tokens[12].kind, TokenKind::And);
    assert_eq!(tokens[13].kind, TokenKind::Or);
    assert_eq!(tokens[14].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_punctuation() {
    let source = "( ) { } [ ] . , ; : -> ...".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::OpenParen);
    assert_eq!(tokens[1].kind, TokenKind::CloseParen);
    assert_eq!(tokens[2].kind, TokenKind::OpenCurly);
    assert_eq!(tokens[3].kind, TokenKind::CloseCurly);
    assert_eq!(tokens[4].kind, TokenKind::OpenBracket);
    assert_eq!(tokens[5].kind, TokenKind::CloseBracket);
    assert_eq!(tokens[6].kind, TokenKind::Dot);
    assert_eq!(tokens[7].kind, TokenKind::Comma);
    assert_eq!(tokens[8].kind, TokenKind::Semicolon);
    assert_eq!(tokens[9].kind, TokenKind::Colon);
    assert_eq!(tokens[10].kind, TokenKind::Arrow);
    assert_eq!(tokens[11].kind, TokenKind::Ellipsis);
    assert_eq!(tokens[12].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_compound_operators() {
    let source = "++ -- += -=".to_string();  // Only test operators that exist
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::PlusPlus);
    assert_eq!(tokens[1].kind, TokenKind::MinusMinus);
    assert_eq!(tokens[2].kind, TokenKind::PlusEquals);
    assert_eq!(tokens[3].kind, TokenKind::MinusEquals);
    assert_eq!(tokens[4].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_comments() {
    let source = "let x = 5 // this is a comment\nlet y = 10".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    // Comments should be skipped
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].value, "x");
    assert_eq!(tokens[2].kind, TokenKind::Assignment);
    assert_eq!(tokens[3].kind, TokenKind::Number);
    assert_eq!(tokens[3].value, "5");
    assert_eq!(tokens[4].kind, TokenKind::Let);
    assert_eq!(tokens[5].kind, TokenKind::Identifier);
    assert_eq!(tokens[5].value, "y");
    assert_eq!(tokens[6].kind, TokenKind::Assignment);
    assert_eq!(tokens[7].kind, TokenKind::Number);
    assert_eq!(tokens[7].value, "10");
    assert_eq!(tokens[8].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_simple_program() {
    let source = "let x = 42;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens.len(), 6); // let, x, =, 42, ;, EOF
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].value, "x");
    assert_eq!(tokens[2].kind, TokenKind::Assignment);
    assert_eq!(tokens[3].kind, TokenKind::Number);
    assert_eq!(tokens[3].value, "42");
    assert_eq!(tokens[4].kind, TokenKind::Semicolon);
    assert_eq!(tokens[5].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_function_declaration() {
    let source = "fn add(a: i32, b: i32) -> i32 { return a + b; }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Fn);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].value, "add");
    assert_eq!(tokens[2].kind, TokenKind::OpenParen);
    assert_eq!(tokens[3].kind, TokenKind::Identifier);
    assert_eq!(tokens[3].value, "a");
    assert_eq!(tokens[4].kind, TokenKind::Colon);
}

#[test]
fn test_tokenize_struct_declaration() {
    let source = "struct Point { x: i32, y: i32 }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Struct);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].value, "Point");
    assert_eq!(tokens[2].kind, TokenKind::OpenCurly);
    assert_eq!(tokens[3].kind, TokenKind::Identifier);
    assert_eq!(tokens[3].value, "x");
}

#[test]
fn test_tokenize_unrecognized_token() {
    let source = "let x = @".to_string();
    let result = tokenize(source, Some("test.lang".to_string()));
    
    assert!(result.is_err());
}

#[test]
fn test_tokenize_whitespace_handling() {
    let source = "  let   x   =   42  ".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    // Whitespace should be skipped
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Assignment);
    assert_eq!(tokens[3].kind, TokenKind::Number);
    assert_eq!(tokens[4].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_newlines() {
    let source = "let x = 1\nlet y = 2\n".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].value, "x");
    assert_eq!(tokens[2].kind, TokenKind::Assignment);
    assert_eq!(tokens[3].kind, TokenKind::Number);
    assert_eq!(tokens[3].value, "1");
    assert_eq!(tokens[4].kind, TokenKind::Let);
    assert_eq!(tokens[5].kind, TokenKind::Identifier);
    assert_eq!(tokens[5].value, "y");
}

#[test]
fn test_tokenize_tilde() {
    let source = "~x".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Tilde);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].value, "x");
    assert_eq!(tokens[2].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_not_operator() {
    let source = "!true".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Not);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].value, "true");
    assert_eq!(tokens[2].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_empty_string() {
    let source = r#""""#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::String);
    assert_eq!(tokens[0].value, "");
    assert_eq!(tokens[1].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_mixed_expression() {
    let source = "x + 5 * (y - 3)".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].kind, TokenKind::Plus);
    assert_eq!(tokens[2].kind, TokenKind::Number);
    assert_eq!(tokens[3].kind, TokenKind::Star);
    assert_eq!(tokens[4].kind, TokenKind::OpenParen);
    assert_eq!(tokens[5].kind, TokenKind::Identifier);
    assert_eq!(tokens[6].kind, TokenKind::Dash);
    assert_eq!(tokens[7].kind, TokenKind::Number);
    assert_eq!(tokens[8].kind, TokenKind::CloseParen);
}

#[test]
fn test_tokenize_typeof() {
    let source = "typeof x".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Typeof);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::EOF);
}

#[test]
fn test_tokenize_question_mark() {
    let source = "x ? y : z".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    
    assert_eq!(tokens[0].kind, TokenKind::Identifier);
    assert_eq!(tokens[1].kind, TokenKind::Question);
    assert_eq!(tokens[2].kind, TokenKind::Identifier);
    assert_eq!(tokens[3].kind, TokenKind::Colon);
    assert_eq!(tokens[4].kind, TokenKind::Identifier);
}
