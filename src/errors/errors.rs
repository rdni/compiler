//! Error type implementations.
//!
//! This module contains the Error and ErrorImpl types that represent
//! compilation errors with source position information. It provides:
//!
//! - Error construction and accessors
//! - Error categorization by type
//! - Helpful error messages and tips
//! - Display formatting for user-friendly error output
//!
//! Errors are used throughout the compilation pipeline (lexing, parsing,
//! type checking, code generation) to report issues to the user.

use std::fmt::Display;

use thiserror::Error;

use crate::Position;

/// Represents a compilation error with position information.
///
/// Contains the specific error variant and the position in the source
/// where the error occurred.
#[derive(Debug, Clone)]
pub struct Error {
    /// The specific error variant
    internal_error: ErrorImpl,
    /// The position in source where the error occurred
    position: Position,
}

impl Error {
    /// Creates a new Error with the given error type and position.
    ///
    /// # Arguments
    ///
    /// * `error_impl` - The specific error variant
    /// * `position` - The source position where the error occurred
    pub fn new(error_impl: ErrorImpl, position: Position) -> Self {
        Error {
            internal_error: error_impl,
            position,
        }
    }

    /// Returns the source position where this error occurred.
    pub fn get_position(&self) -> &Position {
        &self.position
    }

    /// Returns a string name for this error type.
    pub fn get_error_name(&self) -> &str {
        match &self.internal_error {
            ErrorImpl::UnrecognisedToken { .. } => "UnrecognisedToken",
            ErrorImpl::UnexpectedToken { .. } => "UnexpectedToken",
            ErrorImpl::UnexpectedTokenDetailed { .. } => "UnexpectedTokenDetailed",
            ErrorImpl::NumberParseError { .. } => "NumberParseError",
            ErrorImpl::VariableAlreadyDeclared { .. } => "VariableAlreadyDeclared",
            ErrorImpl::VariableNotDeclared { .. } => "VariableNotDeclared",
            ErrorImpl::UnexpectedArguments { .. } => "UnexpectedArguments",
            ErrorImpl::MissingArguments { .. } => "MissingArguments",
            ErrorImpl::ArgumentTypeMatchError { .. } => "ArgumentTypeMatchError",
            ErrorImpl::FieldTypeMatchError { .. } => "FieldTypeMatchError",
            ErrorImpl::TypeMatchError { .. } => "TypeMatchError",
            ErrorImpl::ExpectedExplicitValue => "ExpectedExplicitValue",
            ErrorImpl::FunctionAlreadyDeclared { .. } => "FunctionAlreadyDeclared",
            ErrorImpl::UnknownType { .. } => "UnknownType",
            ErrorImpl::VariableAlreadyDropped { .. } => "VariableAlreadyDropped",
            ErrorImpl::NotImplementedError => "NotImplementedError",
        }
    }

    /// Returns a helpful tip or suggestion for this error.
    ///
    /// Provides context-specific advice to help the user fix the error.
    pub fn get_tip(&self) -> ErrorTip {
        match &self.internal_error {
            ErrorImpl::UnrecognisedToken { .. } => ErrorTip::None,
            ErrorImpl::UnexpectedToken { token } => ErrorTip::Suggestion(format!(
                "Unexpected token: `{}`, did you miss a semicolon?",
                token
            )),
            ErrorImpl::UnexpectedTokenDetailed { token, message } => {
                ErrorTip::Suggestion(format!("Unexpected token: `{}`, {}", token, message))
            }
            ErrorImpl::NumberParseError { token } => ErrorTip::Suggestion(format!(
                "Invalid number: `{}`, is it above the integer limit?",
                token
            )),
            ErrorImpl::VariableAlreadyDeclared { variable } => {
                ErrorTip::Suggestion(format!("Variable `{}` already declared", variable))
            }
            ErrorImpl::VariableNotDeclared { variable } => {
                ErrorTip::Suggestion(format!("Variable `{}` not declared", variable))
            }
            ErrorImpl::UnexpectedArguments { expected, received } => ErrorTip::Suggestion(format!(
                "Expected {} arguments, received {}",
                expected, received
            )),
            ErrorImpl::MissingArguments { expected, received } => ErrorTip::Suggestion(format!(
                "Expected {} arguments, received {}",
                expected, received
            )),
            ErrorImpl::ArgumentTypeMatchError { expected, received } => {
                ErrorTip::Suggestion(format!(
                    "Expected argument type `{}`, received `{}`",
                    expected, received
                ))
            }
            ErrorImpl::FieldTypeMatchError { expected, received } => ErrorTip::Suggestion(format!(
                "Expected field type `{}`, received `{}`",
                expected, received
            )),
            ErrorImpl::TypeMatchError { expected, received } => ErrorTip::Suggestion(format!(
                "Expected type `{}`, received `{}`",
                expected, received
            )),
            ErrorImpl::ExpectedExplicitValue => ErrorTip::Suggestion(String::from(
                "Expected explicit value when no type is given",
            )),
            ErrorImpl::FunctionAlreadyDeclared { function } => {
                ErrorTip::Suggestion(format!("Function `{}` already declared", function))
            }
            ErrorImpl::UnknownType { type_ } => {
                ErrorTip::Suggestion(format!("Unknown type `{}` found", type_))
            }
            ErrorImpl::VariableAlreadyDropped { variable } => {
                ErrorTip::Suggestion(format!("Variable `{}` has already been dropped", variable))
            }
            ErrorImpl::NotImplementedError => ErrorTip::Suggestion(String::from(
                "This feature is expected to be handled, but has not yet been implemented",
            )),
        }
    }
}

pub enum ErrorTip {
    None,
    Suggestion(String),
}

impl Display for ErrorTip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorTip::None => write!(f, ""),
            ErrorTip::Suggestion(suggestion) => write!(f, "{}", suggestion),
        }
    }
}

#[derive(Error, Debug, Clone)]
pub enum ErrorImpl {
    #[error("unrecognised token: {token:?}")]
    UnrecognisedToken { token: String },
    #[error("unexpected token: {token:?}")]
    UnexpectedToken { token: String },
    #[error("unexpected token ({message:?}): {token:?}")]
    UnexpectedTokenDetailed { token: String, message: String },
    #[error("error parsing number: {token:?}")]
    NumberParseError { token: String },
    #[error("variable {variable:?} already declared")]
    VariableAlreadyDeclared { variable: String },
    #[error("variable {variable:?} not declared")]
    VariableNotDeclared { variable: String },
    #[error("unexpected arguments: expected {expected:?}, received {received:?}")]
    UnexpectedArguments { expected: usize, received: usize },
    #[error("unexpected arguments: expected {expected:?}, received {received:?}")]
    MissingArguments { expected: usize, received: usize },
    #[error("argument types do not match: expected {expected:?}, received {received:?}")]
    ArgumentTypeMatchError { expected: String, received: String },
    #[error("field types do not match: expected {expected:?}, received {received:?}")]
    FieldTypeMatchError { expected: String, received: String },
    #[error("types do not match: expected {expected:?}, received {received:?}")]
    TypeMatchError { expected: String, received: String },
    #[error("expected explicit value when no type is given")]
    ExpectedExplicitValue,
    #[error("function {function:?} already declared")]
    FunctionAlreadyDeclared { function: String },
    #[error("unknown type {type_} found")]
    UnknownType { type_: String },
    #[error("variable {variable:?} has already been dropped")]
    VariableAlreadyDropped { variable: String },
    #[error("not implemented error")]
    NotImplementedError,
}
