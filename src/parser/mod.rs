//! Parser module for building an Abstract Syntax Tree (AST).
//!
//! This module contains the parser that transforms a stream of tokens
//! into an Abstract Syntax Tree. It uses a Pratt parser for expressions
//! with proper operator precedence and handles:
//!
//! - Statement parsing (variable declarations, functions, control flow)
//! - Expression parsing (binary ops, function calls, literals)
//! - Type parsing for type annotations
//! - Error recovery and reporting
//!
//! The parser uses NUD (null denotation) and LED (left denotation) functions
//! for expression parsing with binding power for precedence handling.

pub mod expr;
pub mod lookups;
pub mod parser;
pub mod stmt;
pub mod types;
