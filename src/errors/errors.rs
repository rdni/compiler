use thiserror::Error;

use crate::Position;

#[derive(Debug, Clone)]
pub struct Error {
    internal_error: ErrorImpl,
    position: Position
}

impl Error {
    pub fn new(error_impl: ErrorImpl, position: Position) -> Self {
        Error {
            internal_error: error_impl,
            position
        }
    }

    pub fn get_position(&self) -> &Position {
        &self.position
    }

    pub fn get_error_name(&self) -> &str {
        match &self.internal_error {
            ErrorImpl::UnrecognisedToken { .. } => "UnrecognisedToken",
            ErrorImpl::UnexpectedToken { .. } => "UnexpectedToken",
            ErrorImpl::UnexpectedTokenDetailed { .. } => "UnexpectedTokenDetailed",
            ErrorImpl::NumberParseError { .. } => "NumberParseError",
            ErrorImpl::VariableAlreadyDeclared { .. } => "VariableAlreadyDeclared",
            ErrorImpl::UnexpectedArguments { .. } => "UnexpectedArguments",
            ErrorImpl::MissingArguments { .. } => "MissingArguments",
            ErrorImpl::ArgumentTypeMatchError { .. } => "ArgumentTypeMatchError",
            ErrorImpl::FieldTypeMatchError { .. } => "FieldTypeMatchError",
            ErrorImpl::TypeMatchError { .. } => "TypeMatchError",
            ErrorImpl::ExpectedExplicitValue => "ExpectedExplicitValue",
            ErrorImpl::FunctionAlreadyDeclared { .. } => "FunctionAlreadyDeclared",
        }
    }
}

#[derive(Error, Debug, Clone)]
pub enum ErrorImpl {
    #[error("unrecognised token: {token:?}")]
    UnrecognisedToken {
        token: String
    },
    #[error("unexpected token: {token:?}")]
    UnexpectedToken {
        token: String
    },
    #[error("unexpected token ({message:?}): {token:?}")]
    UnexpectedTokenDetailed {
        token: String,
        message: String
    },
    #[error("error parsing number: {token:?}")]
    NumberParseError {
        token: String
    },
    #[error("variable {variable:?} already declared")]
    VariableAlreadyDeclared {
        variable: String
    },
    #[error("unexpected arguments: expected {expected:?}, received {received:?}")]
    UnexpectedArguments {
        expected: usize,
        received: usize
    },
    #[error("unexpected arguments: expected {expected:?}, received {received:?}")]
    MissingArguments {
        expected: usize,
        received: usize
    },
    #[error("argument types do not match: expected {expected:?}, received {received:?}")]
    ArgumentTypeMatchError {
        expected: String,
        received: String
    },
    #[error("field types do not match: expected {expected:?}, received {received:?}")]
    FieldTypeMatchError {
        expected: String,
        received: String
    },
    #[error("types do not match: expected {expected:?}, received {received:?}")]
    TypeMatchError {
        expected: String,
        received: String
    },
    #[error("expected explicit value when no type is given")]
    ExpectedExplicitValue,
    #[error("function {function:?} already declared")]
    FunctionAlreadyDeclared {
        function: String
    },
}