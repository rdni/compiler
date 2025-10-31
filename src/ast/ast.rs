use std::{any::Any, fmt::Debug, ops::Deref};

use crate::type_checker::type_checker::TypeChecker;

use super::{statements::ExpressionStmt, types::Literals};

/// Statement Types
#[derive(PartialEq, Debug)]
pub enum StmtType {
    ExpressionStmt,
    BlockStmt,
    ImportStmt,
    VarDeclStmt,
    IfStmt,
    FnDeclStmt,
    ExternDeclStmt,
    ReturnStmt,
    StructDeclStmt,
    WhileStmt,
    BreakStmt,
    DropStmt,
}

/// Statement Trait
///
/// Defines the behavior of all statement types in the AST.
pub trait Stmt: Debug {
    /// Returns the type of the statement.
    fn get_stmt_type(&self) -> StmtType;
    /// Type conversion purposes - used with `.downcast_ref<T>()`
    fn as_any(&self) -> &dyn Any;
    /// Clones the statement into a StmtWrapper.
    /// Clone cannot be derived for certain trait objects, so this method is necessary.
    fn clone_wrapper(&self) -> StmtWrapper;
    /// Returns the span of the statement.
    fn get_span(&self) -> &crate::Span;
}

/// Statement Wrapper
///
/// A wrapper that allows for any statement kind to be stored with helper methods
#[derive(Debug)]
pub struct StmtWrapper(Box<dyn Stmt>);

impl StmtWrapper {
    pub fn new<T: Stmt + 'static>(stmt: T) -> Self {
        StmtWrapper(Box::new(stmt))
    }
    /// Consumes the wrapper and returns the inner statement
    ///
    /// Should not be used unless absolutely necessary
    #[deprecated(note = "This method consumes the wrapper and is generally not needed")]
    pub fn get_inner(self) -> Box<dyn Stmt> {
        self.0
    }
}

// Not used often, can be removed in future
impl Deref for StmtWrapper {
    type Target = Box<dyn Stmt>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Stmt for StmtWrapper {
    fn get_stmt_type(&self) -> StmtType {
        self.0.get_stmt_type()
    }
    fn as_any(&self) -> &dyn Any {
        self.0.as_any()
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        self.0.clone_wrapper()
    }
    fn get_span(&self) -> &crate::Span {
        self.0.get_span()
    }
}

impl Clone for StmtWrapper {
    fn clone(&self) -> Self {
        self.clone_wrapper()
    }
}

/// Expression Types
///
/// Defines the various kinds of expressions in the AST.
#[derive(PartialEq, Clone, Debug)]
pub enum ExprType {
    Number,
    String,
    Symbol,
    Binary,
    Prefix,
    Assignment,
    CallExpr,
    StructInit,
}

pub trait Expr: Debug {
    /// Returns the expression type of the expression.
    fn get_expr_type(&self) -> ExprType;
    /// Type conversion purposes - used with `.downcast_ref<T>()`
    fn as_any(&self) -> &dyn Any;
    /// Returns the typechecking type of the expression.
    fn get_type(&self, type_checker: &mut TypeChecker) -> TypeWrapper;
    /// Clones the expression into an ExprWrapper.
    /// Clone cannot be derived for certain trait objects, so this method is necessary.
    fn clone_wrapper(&self) -> ExprWrapper;
    /// Returns the span of the expression.
    fn get_span(&self) -> &crate::Span;
}

/// Expression Wrapper
///
/// A wrapper that allows for any expression kind to be stored with helper methods
#[derive(Debug)]
pub struct ExprWrapper(Box<dyn Expr>);

impl ExprWrapper {
    pub fn new<T: Expr + 'static>(expression: T) -> Self {
        ExprWrapper(Box::new(expression))
    }
    /// Does not consume the wrapper - should be renamed in future
    pub fn into_cloned_stmt_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(ExpressionStmt {
            expression: self.0.clone_wrapper(),
            span: self.0.get_span().clone(),
        })
    }
}

impl Expr for ExprWrapper {
    fn as_any(&self) -> &dyn Any {
        self.0.as_any()
    }
    fn get_expr_type(&self) -> ExprType {
        self.0.get_expr_type()
    }
    fn get_type(&self, type_checker: &mut TypeChecker) -> TypeWrapper {
        self.0.get_type(type_checker)
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        self.0.clone_wrapper()
    }
    fn get_span(&self) -> &crate::Span {
        self.0.get_span()
    }
}

/// Type Types
///
/// Defines the various kinds of builtin types in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeType {
    Literal(Literals),
    Array(Box<TypeType>),
    Symbol(String),
    Any,
    Function(String),
    Struct(String),
}

impl TypeType {
    pub fn get_length(&self) -> usize {
        match self {
            TypeType::Literal(Literals::String) => 1,
            _ => panic!("Attempted to get length of non-string type"),
        }
    }

    /// Returns whether the type is an internal type.
    ///
    /// Should only be used in the context of the stdlib.
    pub fn is_internal(&self) -> bool {
        [TypeType::Literal(Literals::InternalI8Pointer)].contains(self)
    }
}

/// Type Trait
///
/// Defines the behavior of all type kinds in the AST.
pub trait Type: Debug {
    /// Clones the type into a TypeWrapper.
    fn clone_wrapper(&self) -> TypeWrapper;
    /// Returns the type type of the type.
    fn get_type_type(&self) -> TypeType;
    /// Type conversion purposes - used with `.downcast_ref<T>()`
    fn as_any(&self) -> &dyn Any;
    /// Returns a list of method names available on the type.
    ///
    /// Currently not used - needs to be implemented
    fn get_methods(&self) -> Vec<String>;
    /// Returns the type of a property or method on the type.
    ///
    /// Note: This includes methods as well as properties.
    fn get_property_type(&self, property: String) -> TypeWrapper; // Includes methods
    /// Checks if the type is compatible with another type.
    ///
    /// This may mean two different integers are compatible, for example.
    fn is_compatible_with(&self, other: &TypeType) -> bool;
    /// Returns whether the type has an internal destructor.
    ///
    /// Currenttly only strings have these, and the syntax is not defined yet.
    fn has_internal_destructor(&self) -> bool;
}

/// Type Wrapper
///
/// A wrapper that allows for any type kind to be stored with helper methods
#[derive(Debug)]
pub struct TypeWrapper(Box<dyn Type>);

impl TypeWrapper {
    pub fn new<T: Type + 'static>(type_: T) -> Self {
        TypeWrapper(Box::new(type_))
    }
    /// Consumes the wrapper and returns the inner type
    pub fn get_inner(self) -> Box<dyn Type> {
        self.0
    }
}

impl Type for TypeWrapper {
    fn clone_wrapper(&self) -> TypeWrapper {
        self.0.clone_wrapper()
    }
    fn get_type_type(&self) -> TypeType {
        self.0.get_type_type()
    }
    fn as_any(&self) -> &dyn Any {
        self.0.as_any()
    }
    fn get_methods(&self) -> Vec<String> {
        self.0.get_methods()
    }
    fn get_property_type(&self, property: String) -> TypeWrapper {
        self.0.get_property_type(property)
    }
    fn is_compatible_with(&self, other: &TypeType) -> bool {
        self.0.is_compatible_with(other)
    }
    fn has_internal_destructor(&self) -> bool {
        self.0.has_internal_destructor()
    }
}
