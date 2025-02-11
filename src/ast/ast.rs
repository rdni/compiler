use std::{any::Any, fmt::Debug, ops::Deref};

use crate::type_checker::type_checker::TypeChecker;

use super::{statements::ExpressionStmt, types::Literals};

#[derive(PartialEq, Debug)]
pub enum StmtType {
    ExpressionStmt,
    BlockStmt,
    ImportStmt,
    VarDeclStmt,
    IfStmt,
    FnDeclStmt,
    ReturnStmt,
    StructDeclStmt
}

pub trait Stmt: Debug {
    fn get_stmt_type(&self) -> StmtType;
    fn as_any(&self) -> &dyn Any; // Type conversion purposes
    fn clone_wrapper(&self) -> StmtWrapper;
    fn get_span(&self) -> &crate::Span;
}

#[derive(Debug)]
pub struct StmtWrapper(Box<dyn Stmt>);

impl StmtWrapper {
    pub fn new<T: Stmt + 'static>(stmt: T) -> Self {
        StmtWrapper(Box::new(stmt))
    }
    pub fn get_inner(self) -> Box<dyn Stmt> {
        self.0
    }
}

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
    fn get_expr_type(&self) -> ExprType;
    fn as_any(&self) -> &dyn Any;
    fn get_type(&self, type_checker: &mut TypeChecker) -> TypeWrapper;
    fn clone_wrapper(&self) -> ExprWrapper;
    fn get_span(&self) -> &crate::Span;
}

#[derive(Debug)]
pub struct ExprWrapper(Box<dyn Expr>);

impl ExprWrapper {
    pub fn new<T: Expr + 'static>(expression: T) -> Self {
        ExprWrapper(Box::new(expression))
    }
    pub fn into_cloned_stmt_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(ExpressionStmt { expression: self.0.clone_wrapper(), span: self.0.get_span().clone() })
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

#[derive(Debug, Clone, PartialEq)]
pub enum TypeType {
    Literal(Literals),
    Array(Box<TypeType>),
    Symbol(String),
    Any,
    Function(String),
    Struct(String)
}

impl TypeType {
    pub fn get_length(&self) -> usize {
        match self {
            TypeType::Literal(Literals::String) => 1,
            _ => panic!("Attempted to get length of non-string type")
        }
    }
}

pub trait Type: Debug {
    fn clone_wrapper(&self) -> TypeWrapper;
    fn get_type_type(&self) -> TypeType;
    fn as_any(&self) -> &dyn Any;
    fn get_methods(&self) -> Vec<String>;
    fn get_property_type(&self, property: String) -> TypeWrapper; // Includes methods
}

#[derive(Debug)]
pub struct TypeWrapper(Box<dyn Type>);

impl TypeWrapper {
    pub fn new<T: Type + 'static>(type_: T) -> Self {
        TypeWrapper(Box::new(type_))
    }
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
}