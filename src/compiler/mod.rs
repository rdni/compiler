//! Code generation module for the compiler.
//!
//! This module contains the LLVM-based code generator that transforms
//! the typed AST into executable LLVM IR. It handles:
//!
//! - Compilation of expressions and statements
//! - Type conversion from AST types to LLVM types
//! - Standard library integration
//! - Function and struct code generation

pub mod compiler;
pub mod expr;
pub mod stdlib;
pub mod stmt;
