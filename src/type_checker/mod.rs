//! Type checking and semantic analysis module.
//!
//! This module performs type checking and semantic analysis on the AST.
//! It transforms the untyped AST into a typed AST while:
//!
//! - Verifying type correctness of expressions and statements
//! - Resolving variable and function references
//! - Checking function signatures and argument types
//! - Managing scopes and environments
//! - Detecting use of undeclared or dropped variables
//!
//! The type checker maintains environment stacks to handle nested scopes
//! and validates that all operations are type-safe.

pub mod type_checker;
pub mod typed_ast;
