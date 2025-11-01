//! Type system definitions for the AST.
//!
//! This module defines the type system used in the language, including:
//!
//! - Primitive types (numbers, strings, booleans)
//! - Composite types (arrays, structs, functions)
//! - Type wrappers and conversions
//! - Literal type representations
//!
//! Types in the AST are used during parsing to represent type annotations,
//! and are then refined by the type checker into concrete types.

use core::panic;
use std::{any::Any, rc::Rc};

use crate::{type_checker::typed_ast::TypedBlockStmt, Position};

use super::ast::{Type, TypeType, TypeWrapper};

/// Represents the different literal types in the language.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literals {
    String,
    Number(NumberType),
    Boolean,
    Null,
    InternalI8Pointer,
}

/// Represents numeric types with different precisions.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumberType {
    Int32,
    Int64,
    Int8,
    Int16,
}

/// Represents a symbolic type reference that needs resolution.
///
/// During parsing, type references are stored as symbols and later
/// resolved to concrete types during type checking.
#[derive(Debug, Clone)]
pub struct SymbolType {
    pub name: String,
    pub position: Position,
}

impl SymbolType {
    pub fn get_position(&self) -> Position {
        self.position.clone()
    }
}

impl Type for SymbolType {
    fn clone_wrapper(&self) -> TypeWrapper {
        TypeWrapper::new(self.clone())
    }
    fn get_type_type(&self) -> TypeType {
        TypeType::Symbol(self.name.clone())
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    // These methods should never be called on SymbolType
    fn get_methods(&self) -> Vec<String> {
        // No methods for symbols (not a real type)
        panic!("Attempted to get methods for symbol type")
    }
    fn get_property_type(&self, _: String) -> TypeWrapper {
        // No properties for symbols (not a real type)
        panic!("Attempted to get properties for symbol type")
    }
    fn is_compatible_with(&self, _other: &TypeType) -> bool {
        panic!("Attempted to check compatibility for symbol type")
    }
    fn has_internal_destructor(&self) -> bool {
        panic!("Attempted to check internal destructor for symbol type")
    }
}

/// Represents an array type
/// Currently not implemented
#[derive(Debug)]
pub struct ArrayType {
    pub underlying: TypeWrapper, // []T
}

impl Type for ArrayType {
    fn clone_wrapper(&self) -> TypeWrapper {
        TypeWrapper::new(ArrayType {
            underlying: self.underlying.clone_wrapper(),
        })
    }
    fn get_type_type(&self) -> TypeType {
        TypeType::Array(Box::new(self.underlying.get_type_type()))
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_methods(&self) -> Vec<String> {
        // index, len, push, pop
        vec![
            "index".to_string(),
            "len".to_string(),
            "push".to_string(),
            "pop".to_string(),
        ]
    }
    fn get_property_type(&self, property: String) -> TypeWrapper {
        // index: fn(index: Number) -> T
        // len: fn() -> Number
        // push: fn(value: T) -> ()
        // pop: fn() -> T
        match &*property {
            "index" => TypeWrapper::new(FunctionType {
                name: "index".to_string(),
                is_native: true,
                body: None,
                return_type: self.underlying.clone_wrapper(),
                arguments: vec![
                    ("self".to_string(), self.clone_wrapper()),
                    (
                        "index".to_string(),
                        TypeWrapper::new(LiteralType {
                            literal: Literals::Number(NumberType::Int32),
                        }),
                    ),
                ],
                is_var_args: false,
                should_export: false,
            }),
            "len" => TypeWrapper::new(FunctionType {
                name: "len".to_string(),
                is_native: true,
                body: None,
                return_type: TypeWrapper::new(LiteralType {
                    literal: Literals::Number(NumberType::Int32),
                }),
                arguments: vec![("self".to_string(), self.clone_wrapper())],
                is_var_args: false,
                should_export: false,
            }),
            "push" => TypeWrapper::new(FunctionType {
                name: "push".to_string(),
                is_native: true,
                body: None,
                return_type: TypeWrapper::new(AnyType),
                arguments: vec![
                    ("self".to_string(), self.clone_wrapper()),
                    ("value".to_string(), self.underlying.clone_wrapper()),
                ],
                is_var_args: false,
                should_export: false,
            }),
            "pop" => TypeWrapper::new(FunctionType {
                name: "pop".to_string(),
                is_native: true,
                body: None,
                return_type: self.underlying.clone_wrapper(),
                arguments: vec![("self".to_string(), self.clone_wrapper())],
                is_var_args: false,
                should_export: false,
            }),
            _ => panic!("Attempted to get property type for non-existent property"),
        }
    }
    fn is_compatible_with(&self, _other: &TypeType) -> bool {
        todo!();
    }
    fn has_internal_destructor(&self) -> bool {
        true
    }
}

/// Represents a literal type
#[derive(Debug, Clone)]
pub struct LiteralType {
    pub literal: Literals,
}

impl Type for LiteralType {
    fn clone_wrapper(&self) -> TypeWrapper {
        TypeWrapper::new(self.clone())
    }
    fn get_type_type(&self) -> TypeType {
        TypeType::Literal(self.literal)
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_methods(&self) -> Vec<String> {
        // Depends on the literal
        match self.literal {
            Literals::String => vec!["len".to_string()],
            Literals::Number(_) => vec![],
            Literals::Boolean => vec![],
            Literals::Null => vec![],
            Literals::InternalI8Pointer => vec![],
        }
    }
    fn get_property_type(&self, property: String) -> TypeWrapper {
        // Depends on the literal
        match self.literal {
            Literals::String => match &*property {
                "len" => TypeWrapper::new(FunctionType {
                    name: "len".to_string(),
                    is_native: true,
                    body: None,
                    return_type: TypeWrapper::new(LiteralType {
                        literal: Literals::Number(NumberType::Int32),
                    }),
                    arguments: vec![("self".to_string(), self.clone_wrapper())],
                    is_var_args: false,
                    should_export: false,
                }),
                _ => panic!("Attempted to get property type for non-existent property"),
            },
            _ => {
                panic!("Attempted to get property type for a literal that doesn't have properties")
            }
        }
    }
    fn is_compatible_with(&self, other: &TypeType) -> bool {
        match self.get_type_type() {
            TypeType::Literal(Literals::Number(_)) => match other {
                TypeType::Literal(Literals::Number(_)) => true,
                _ => false,
            },
            TypeType::Literal(Literals::String) => match other {
                TypeType::Literal(Literals::InternalI8Pointer) => true,
                _ => false,
            },
            TypeType::Literal(Literals::InternalI8Pointer) => match other {
                TypeType::Literal(Literals::String) => true,
                _ => false,
            },
            _ => false,
        }
    }
    fn has_internal_destructor(&self) -> bool {
        match self.literal {
            Literals::String => true,
            _ => false,
        }
    }
}

/// Represents the any type
/// Currently not implemented
///
/// This will be used for casting (should act a bit like a void* in C).
#[derive(Debug, Clone)]
pub struct AnyType;

impl Type for AnyType {
    fn clone_wrapper(&self) -> TypeWrapper {
        TypeWrapper::new(self.clone())
    }
    fn get_type_type(&self) -> TypeType {
        TypeType::Any
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_methods(&self) -> Vec<String> {
        // No methods for any
        panic!("Attempted to get methods for any type")
    }
    fn get_property_type(&self, _: String) -> TypeWrapper {
        // No properties for any
        panic!("Attempted to get properties for any type")
    }
    fn is_compatible_with(&self, _other: &TypeType) -> bool {
        true
    }
    fn has_internal_destructor(&self) -> bool {
        false
    }
}

/// Represents a function type
#[derive(Debug)]
pub struct FunctionType {
    pub name: String,
    /// Whether this function is native
    /// This includes functions declared with `extern` or stdlib functions outside of the stdlib compilation process
    pub is_native: bool,
    pub body: Option<Rc<TypedBlockStmt>>, // Native functions will get magically handled
    pub return_type: TypeWrapper,
    pub arguments: Vec<(String, TypeWrapper)>,
    /// Whether this function accepts variable arguments
    /// Only printf has this, and that has been phased out
    pub is_var_args: bool,
    /// Whether this function should be exported
    /// By default, most functions are exported
    pub should_export: bool,
}

impl Type for FunctionType {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> TypeWrapper {
        TypeWrapper::new(FunctionType {
            name: self.name.clone(),
            is_native: self.is_native,
            body: self.body.clone(),
            return_type: self.return_type.clone_wrapper(),
            arguments: self
                .arguments
                .iter()
                .map(|x| (x.0.clone(), x.1.clone_wrapper()))
                .collect::<Vec<(String, TypeWrapper)>>(),
            is_var_args: self.is_var_args,
            should_export: self.should_export,
        })
    }
    fn get_type_type(&self) -> TypeType {
        TypeType::Function(self.name.clone())
    }
    fn get_methods(&self) -> Vec<String> {
        // call: fn(self, args: self.arguments) -> self.return_type
        vec!["call".to_string()]
    }
    fn get_property_type(&self, property: String) -> TypeWrapper {
        // call: fn(self, args: self.arguments) -> self.return_type
        match &*property {
            "call" => TypeWrapper::new(FunctionType {
                name: "call".to_string(),
                is_native: true,
                body: None,
                return_type: self.return_type.clone_wrapper(),
                arguments: vec![
                    ("self".to_string(), self.clone_wrapper()),
                    (
                        "args".to_string(),
                        TypeWrapper::new(ArrayType {
                            underlying: TypeWrapper::new(AnyType),
                        }),
                    ),
                ],
                is_var_args: false,
                should_export: false,
            }),
            _ => panic!(
                "Attempted to get property type for non-existent property {}",
                property
            ),
        }
    }
    fn is_compatible_with(&self, _other: &TypeType) -> bool {
        false
    }
    fn has_internal_destructor(&self) -> bool {
        false
    }
}

impl Clone for FunctionType {
    fn clone(&self) -> Self {
        FunctionType {
            name: self.name.clone(),
            is_native: self.is_native,
            body: self.body.clone(),
            return_type: self.return_type.clone_wrapper(),
            arguments: self
                .arguments
                .iter()
                .map(|x| (x.0.clone(), x.1.clone_wrapper()))
                .collect::<Vec<(String, TypeWrapper)>>(),
            is_var_args: self.is_var_args,
            should_export: self.should_export,
        }
    }
}

/// Struct type
#[derive(Debug)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, TypeWrapper)>,
}

impl Type for StructType {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> TypeWrapper {
        TypeWrapper::new(StructType {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(id, ty)| (id.clone(), ty.clone_wrapper()))
                .collect(),
        })
    }
    fn get_type_type(&self) -> TypeType {
        TypeType::Struct(self.name.clone())
    }
    fn get_methods(&self) -> Vec<String> {
        // No methods for structs
        panic!("Attempted to get methods for struct type")
    }
    fn get_property_type(&self, property: String) -> TypeWrapper {
        // Get the type of the property
        let prop = self.fields.iter().find(|(id, _)| id == &property);
        if let Some((_, ty)) = prop {
            ty.clone_wrapper()
        } else {
            panic!("Attempted to get property type for non-existent property")
        }
    }
    fn is_compatible_with(&self, _other: &TypeType) -> bool {
        false
    }
    fn has_internal_destructor(&self) -> bool {
        false
    }
}
