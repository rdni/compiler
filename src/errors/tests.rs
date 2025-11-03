//! Unit tests for error handling.
//!
//! This module contains tests for error types and error reporting.

use crate::errors::errors::{Error, ErrorImpl, ErrorTip};
use crate::Position;
use std::rc::Rc;

#[test]
fn test_error_creation() {
    let error = Error::new(
        ErrorImpl::UnrecognisedToken {
            token: "@".to_string(),
        },
        Position(10, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "UnrecognisedToken");
}

#[test]
fn test_error_position() {
    let pos = Position(42, Rc::new("test.lang".to_string()));
    let error = Error::new(
        ErrorImpl::UnexpectedToken {
            token: "identifier".to_string(),
        },
        pos.clone(),
    );

    assert_eq!(error.get_position().0, 42);
}

#[test]
fn test_unexpected_token_error() {
    let error = Error::new(
        ErrorImpl::UnexpectedToken {
            token: "identifier".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "UnexpectedToken");
}

#[test]
fn test_type_mismatch_error() {
    let error = Error::new(
        ErrorImpl::TypeMatchError {
            expected: "i32".to_string(),
            received: "string".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "TypeMatchError");
}

#[test]
fn test_variable_not_declared_error() {
    let error = Error::new(
        ErrorImpl::VariableNotDeclared {
            variable: "foo".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "VariableNotDeclared");
}

#[test]
fn test_variable_already_declared_error() {
    let error = Error::new(
        ErrorImpl::VariableAlreadyDeclared {
            variable: "x".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "VariableAlreadyDeclared");
}

#[test]
fn test_function_already_declared_error() {
    let error = Error::new(
        ErrorImpl::FunctionAlreadyDeclared {
            function: "main".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "FunctionAlreadyDeclared");
}

#[test]
fn test_unknown_type_error() {
    let error = Error::new(
        ErrorImpl::UnknownType {
            type_: "CustomType".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "UnknownType");
}

#[test]
fn test_error_tip_none() {
    let error = Error::new(
        ErrorImpl::UnrecognisedToken {
            token: "@".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert!(matches!(error.get_tip(), ErrorTip::None));
}

#[test]
fn test_error_tip_suggestion() {
    let error = Error::new(
        ErrorImpl::UnexpectedToken {
            token: "}".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    match error.get_tip() {
        ErrorTip::Suggestion(_) => (),
        _ => panic!("Expected suggestion tip"),
    }
}

#[test]
fn test_error_tip_display() {
    let tip = ErrorTip::Suggestion("Try this instead".to_string());
    assert_eq!(tip.to_string(), "Try this instead");

    let tip = ErrorTip::None;
    assert_eq!(tip.to_string(), "");
}

#[test]
fn test_unexpected_arguments_error() {
    let error = Error::new(
        ErrorImpl::UnexpectedArguments {
            expected: 2,
            received: 3,
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "UnexpectedArguments");
}

#[test]
fn test_missing_arguments_error() {
    let error = Error::new(
        ErrorImpl::MissingArguments {
            expected: 3,
            received: 1,
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "MissingArguments");
}

#[test]
fn test_variable_already_dropped_error() {
    let error = Error::new(
        ErrorImpl::VariableAlreadyDropped {
            variable: "x".to_string(),
        },
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "VariableAlreadyDropped");
}

#[test]
fn test_not_implemented_error() {
    let error = Error::new(
        ErrorImpl::NotImplementedError,
        Position(0, Rc::new("test.lang".to_string())),
    );

    assert_eq!(error.get_error_name(), "NotImplementedError");
}
