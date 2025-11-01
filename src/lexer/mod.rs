//! Lexical analysis module for the compiler.
//!
//! This module contains the lexer (tokenizer) that converts source code
//! into a stream of tokens for parsing. It handles:
//!
//! - Tokenization of source code using regex patterns
//! - Recognition of keywords, identifiers, literals, and operators
//! - Token position tracking for error reporting
//! - Comments and whitespace handling

pub mod lexer;
pub mod tokens;
