//! Typed Abstract Syntax Tree definitions.
//!
//! This module contains the typed variants of AST nodes produced by
//! the type checker. Each typed node includes type information and
//! has been validated for type correctness.
//!
//! The typed AST mirrors the structure of the untyped AST but includes:
//! - Resolved types for all expressions
//! - Validated function signatures
//! - Verified variable references
//! - Type-checked operations
//!
//! This typed AST is consumed by the code generator to produce LLVM IR.

use std::any::Any;

use crate::{
    ast::{
        ast::{Expr, ExprType, ExprWrapper, Stmt, StmtType, StmtWrapper, Type, TypeWrapper},
        statements::ExpressionStmt,
        types::{FunctionType, LiteralType, Literals, StructType},
    },
    lexer::tokens::{Token, TokenKind},
    Span,
};

use super::type_checker::TypeChecker;

/// Trait for typed statement nodes.
///
/// Extends the Stmt trait with typed-specific functionality.
pub trait TypedStmt: Stmt {
    /// Clones this statement into a TypedStmtWrapper.
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper;
}

/// Wrapper for typed statement nodes.
///
/// Provides a uniform interface for all typed statement types.
#[derive(Debug)]
pub struct TypedStmtWrapper(Box<dyn TypedStmt>);

impl TypedStmtWrapper {
    /// Creates a new TypedStmtWrapper around a typed statement.
    pub fn new<T: TypedStmt + 'static>(stmt: T) -> Self {
        TypedStmtWrapper(Box::new(stmt))
    }
    
    /// Consumes the wrapper and returns the inner typed statement.
    pub fn get_inner(self) -> Box<dyn TypedStmt> {
        self.0
    }
}

impl Stmt for TypedStmtWrapper {
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

impl TypedStmt for TypedStmtWrapper {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        self.0.clone_typed_wrapper()
    }
}

#[derive(Debug)]
pub struct TypedExpressionStmt {
    pub expression: TypedExprWrapper,
}

impl Stmt for TypedExpressionStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedExpressionStmt {
            expression: self.expression.clone_typed_wrapper(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::ExpressionStmt
    }
    fn get_span(&self) -> &crate::Span {
        self.expression.get_span()
    }
}

impl TypedStmt for TypedExpressionStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedExpressionStmt {
            expression: self.expression.clone_typed_wrapper(),
        })
    }
}

#[derive(Debug)]
pub struct TypedVarDeclStmt {
    pub identifier: String,
    pub is_constant: bool,
    pub assigned_value: Option<TypedExprWrapper>,
    pub var_type: TypeWrapper,
    pub span: crate::Span,
}

impl Stmt for TypedVarDeclStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedVarDeclStmt {
            identifier: self.identifier.clone(),
            is_constant: self.is_constant,
            assigned_value: if self.assigned_value.is_none() {
                None
            } else {
                Some(self.assigned_value.as_ref().unwrap().clone_typed_wrapper())
            },
            var_type: self.var_type.clone_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::VarDeclStmt
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

impl TypedStmt for TypedVarDeclStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedVarDeclStmt {
            identifier: self.identifier.clone(),
            is_constant: self.is_constant,
            assigned_value: if self.assigned_value.is_none() {
                None
            } else {
                Some(self.assigned_value.as_ref().unwrap().clone_typed_wrapper())
            },
            var_type: self.var_type.clone_wrapper(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedBlockStmt {
    pub body: Vec<TypedStmtWrapper>,
    pub id: i32,
    pub span: crate::Span,
}

impl Stmt for TypedBlockStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedBlockStmt {
            body: self
                .body
                .iter()
                .map(|stmt| stmt.clone_typed_wrapper())
                .collect(),
            id: self.id,
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::BlockStmt
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

impl TypedStmt for TypedBlockStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedBlockStmt {
            body: self
                .body
                .iter()
                .map(|stmt| stmt.clone_typed_wrapper())
                .collect(),
            id: self.id,
            span: self.span.clone(),
        })
    }
}

impl Clone for TypedBlockStmt {
    fn clone(&self) -> Self {
        TypedBlockStmt {
            body: self
                .body
                .iter()
                .map(|stmt| stmt.clone_typed_wrapper())
                .collect(),
            id: self.id,
            span: self.span.clone(),
        }
    }
}

// Expressions
pub trait TypedExpr: Expr {
    fn get_type(&self) -> TypeWrapper;
    fn clone_typed_wrapper(&self) -> TypedExprWrapper;
    fn is_wrapper(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct TypedExprWrapper(Box<dyn TypedExpr>);

impl TypedExprWrapper {
    pub fn new<T: TypedExpr + 'static>(expression: T) -> Self {
        TypedExprWrapper(Box::new(expression))
    }
    pub fn into_cloned_stmt_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(ExpressionStmt {
            expression: self.0.clone_wrapper(),
            span: self.get_span().clone(),
        })
    }
    pub fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper(Box::new(self.0.clone_typed_wrapper()))
    }
    pub fn get_inner(self) -> Box<dyn TypedExpr> {
        self.0
    }
}

impl Expr for TypedExprWrapper {
    fn as_any(&self) -> &dyn Any {
        self.0.as_any()
    }
    fn get_expr_type(&self) -> ExprType {
        self.0.get_expr_type()
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        TypedExpr::get_type(&*self.0)
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        self.0.clone_wrapper()
    }
    fn get_span(&self) -> &crate::Span {
        self.0.get_span()
    }
}

impl TypedExpr for TypedExprWrapper {
    fn get_type(&self) -> TypeWrapper {
        TypedExpr::get_type(&*self.0)
    }
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        self.0.clone_typed_wrapper()
    }
    fn is_wrapper(&self) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct TypedBinaryExpr {
    pub left: TypedExprWrapper,
    pub operator: Token,
    pub right: TypedExprWrapper,
    pub span: Span,
}

impl Expr for TypedBinaryExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Binary
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        if self.operator.value == "." {
            TypedExpr::get_type(&self.left).get_property_type(
                self.right
                    .as_any()
                    .downcast_ref::<TypedSymbolExpr>()
                    .unwrap()
                    .value
                    .clone(),
            )
        } else {
            TypedExpr::get_type(&self.left)
        }
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(TypedBinaryExpr {
            left: self.left.clone_typed_wrapper(),
            operator: self.operator.clone(),
            right: self.right.clone_typed_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedExpr for TypedBinaryExpr {
    fn get_type(&self) -> TypeWrapper {
        if self.operator.kind == TokenKind::And
            || self.operator.kind == TokenKind::Or
            || self.operator.kind == TokenKind::Equals
            || self.operator.kind == TokenKind::NotEquals
            || self.operator.kind == TokenKind::Less
            || self.operator.kind == TokenKind::LessEquals
            || self.operator.kind == TokenKind::Greater
            || self.operator.kind == TokenKind::GreaterEquals
        {
            TypeWrapper::new(LiteralType {
                literal: Literals::Boolean,
            })
        } else if self.operator.value == "." {
            TypedExpr::get_type(&self.left).get_property_type(
                self.right
                    .as_any()
                    .downcast_ref::<TypedSymbolExpr>()
                    .unwrap()
                    .value
                    .clone(),
            )
        } else {
            TypedExpr::get_type(&self.left)
        }
    }
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper::new(TypedBinaryExpr {
            left: self.left.clone_typed_wrapper(),
            operator: self.operator.clone(),
            right: self.right.clone_typed_wrapper(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedPrefixExpr {
    pub operator: String,
    pub right_expr: TypedExprWrapper,
    pub span: Span,
}

impl Expr for TypedPrefixExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Prefix
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        TypedExpr::get_type(&self.right_expr)
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(TypedPrefixExpr {
            operator: self.operator.clone(),
            right_expr: self.right_expr.clone_typed_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedExpr for TypedPrefixExpr {
    fn get_type(&self) -> TypeWrapper {
        TypedExpr::get_type(&self.right_expr)
    }
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper::new(TypedPrefixExpr {
            operator: self.operator.clone(),
            right_expr: self.right_expr.clone_typed_wrapper(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedAssignmentExpr {
    pub assignee: TypedExprWrapper,
    pub operator: Token,
    pub value: TypedExprWrapper,
    pub span: Span,
    pub value_type: TypeWrapper,
}

impl Expr for TypedAssignmentExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Assignment
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        TypedExpr::get_type(&self.value)
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(TypedAssignmentExpr {
            assignee: self.assignee.clone_typed_wrapper(),
            operator: self.operator.clone(),
            value: self.value.clone_typed_wrapper(),
            span: self.span.clone(),
            value_type: self.value_type.clone_wrapper(),
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedExpr for TypedAssignmentExpr {
    fn get_type(&self) -> TypeWrapper {
        TypedExpr::get_type(&self.value)
    }
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper::new(TypedAssignmentExpr {
            assignee: self.assignee.clone_typed_wrapper(),
            operator: self.operator.clone(),
            value: self.value.clone_typed_wrapper(),
            span: self.span.clone(),
            value_type: self.value_type.clone_wrapper(),
        })
    }
}

#[derive(Debug)]
pub struct TypedCallExpr {
    pub callee: FunctionType,
    pub arguments: Vec<TypedExprWrapper>,
    pub span: Span,
}

impl Expr for TypedCallExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::CallExpr
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        self.callee.clone_wrapper()
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(TypedCallExpr {
            callee: self.callee.clone(),
            arguments: self
                .arguments
                .iter()
                .map(|arg| arg.clone_typed_wrapper())
                .collect(),
            span: self.span.clone(),
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedExpr for TypedCallExpr {
    fn get_type(&self) -> TypeWrapper {
        self.callee.return_type.clone_wrapper()
    }
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper::new(TypedCallExpr {
            callee: self.callee.clone(),
            arguments: self
                .arguments
                .iter()
                .map(|arg| arg.clone_typed_wrapper())
                .collect(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedSymbolExpr {
    pub value: String,
    pub var_type: TypeWrapper,
    pub span: Span,
}

impl Expr for TypedSymbolExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Symbol
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        self.var_type.clone_wrapper()
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(TypedSymbolExpr {
            value: self.value.clone(),
            var_type: self.var_type.clone_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedExpr for TypedSymbolExpr {
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper::new(TypedSymbolExpr {
            value: self.value.clone(),
            var_type: self.var_type.clone_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_type(&self) -> TypeWrapper {
        self.var_type.clone_wrapper()
    }
}

#[derive(Debug)]
pub struct TypedIfStmt {
    pub condition: TypedExprWrapper,
    pub then_body: TypedStmtWrapper,
    pub else_body: Option<TypedStmtWrapper>,
    pub span: Span,
}

impl Stmt for TypedIfStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedIfStmt {
            condition: self.condition.clone_typed_wrapper(),
            then_body: self.then_body.clone_typed_wrapper(),
            else_body: if self.else_body.is_none() {
                None
            } else {
                Some(self.else_body.as_ref().unwrap().clone_typed_wrapper())
            },
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::IfStmt
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for TypedIfStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedIfStmt {
            condition: self.condition.clone_typed_wrapper(),
            then_body: self.then_body.clone_typed_wrapper(),
            else_body: if self.else_body.is_none() {
                None
            } else {
                Some(self.else_body.as_ref().unwrap().clone_typed_wrapper())
            },
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedExternDeclStmt {
    pub identifier: String,
    pub parameters: Vec<(String, TypeWrapper)>,
    pub return_type: TypeWrapper,
    pub is_variadic: bool,
    pub span: Span,
}

impl Stmt for TypedExternDeclStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedExternDeclStmt {
            identifier: self.identifier.clone(),
            parameters: self
                .parameters
                .iter()
                .map(|(id, ty)| (id.clone(), ty.clone_wrapper()))
                .collect(),
            return_type: self.return_type.clone_wrapper(),
            is_variadic: self.is_variadic,
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::ExternDeclStmt
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for TypedExternDeclStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedExternDeclStmt {
            identifier: self.identifier.clone(),
            parameters: self
                .parameters
                .iter()
                .map(|(id, ty)| (id.clone(), ty.clone_wrapper()))
                .collect(),
            is_variadic: self.is_variadic,
            return_type: self.return_type.clone_wrapper(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedFnDeclStmt {
    pub identifier: String,
    pub parameters: Vec<(String, TypeWrapper)>,
    pub return_type: TypeWrapper,
    pub body: TypedBlockStmt,
    pub span: Span,
}

impl Stmt for TypedFnDeclStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedFnDeclStmt {
            identifier: self.identifier.clone(),
            parameters: self
                .parameters
                .iter()
                .map(|(id, ty)| (id.clone(), ty.clone_wrapper()))
                .collect(),
            return_type: self.return_type.clone_wrapper(),
            body: self.body.clone(),
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::FnDeclStmt
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for TypedFnDeclStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedFnDeclStmt {
            identifier: self.identifier.clone(),
            parameters: self
                .parameters
                .iter()
                .map(|(id, ty)| (id.clone(), ty.clone_wrapper()))
                .collect(),
            return_type: self.return_type.clone_wrapper(),
            body: self.body.clone(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedReturnStmt {
    pub value: Option<TypedExprWrapper>,
    pub span: Span,
}

impl Stmt for TypedReturnStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedReturnStmt {
            value: if self.value.is_none() {
                None
            } else {
                Some(self.value.as_ref().unwrap().clone_typed_wrapper())
            },
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::ReturnStmt
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for TypedReturnStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedReturnStmt {
            value: if self.value.is_none() {
                None
            } else {
                Some(self.value.as_ref().unwrap().clone_typed_wrapper())
            },
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedStructDeclStmt {
    pub name: String,
    pub fields: Vec<(String, TypeWrapper)>,
    pub span: Span,
}

impl Stmt for TypedStructDeclStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedStructDeclStmt {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(id, ty)| (id.clone(), ty.clone_wrapper()))
                .collect(),
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::StructDeclStmt
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for TypedStructDeclStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedStructDeclStmt {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(id, ty)| (id.clone(), ty.clone_wrapper()))
                .collect(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedStructInitExpr {
    pub name: String,
    pub fields: Vec<(String, TypedExprWrapper)>,
    pub span: Span,
}

impl Expr for TypedStructInitExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::StructInit
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        TypeWrapper::new(StructType {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(id, expr)| (id.clone(), TypedExpr::get_type(expr)))
                .collect(),
        })
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(TypedStructInitExpr {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(id, expr)| (id.clone(), expr.clone_typed_wrapper()))
                .collect(),
            span: self.span.clone(),
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedExpr for TypedStructInitExpr {
    fn get_type(&self) -> TypeWrapper {
        TypeWrapper::new(StructType {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(id, expr)| (id.clone(), TypedExpr::get_type(expr)))
                .collect(),
        })
    }
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper::new(TypedStructInitExpr {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(id, expr)| (id.clone(), expr.clone_typed_wrapper()))
                .collect(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedBreakStmt {
    pub span: Span,
}

impl Stmt for TypedBreakStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedBreakStmt {
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::BreakStmt
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for TypedBreakStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedBreakStmt {
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedWhileStmt {
    pub condition: TypedExprWrapper,
    pub body: TypedStmtWrapper,
    pub span: Span,
}

impl Stmt for TypedWhileStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedWhileStmt {
            condition: self.condition.clone_typed_wrapper(),
            body: self.body.clone_typed_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::WhileStmt
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for TypedWhileStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedWhileStmt {
            condition: self.condition.clone_typed_wrapper(),
            body: self.body.clone_typed_wrapper(),
            span: self.span.clone(),
        })
    }
}

#[derive(Debug)]
pub struct TypedDropStmt {
    pub expression: TypedExprWrapper,
    pub span: Span,
}

impl Stmt for TypedDropStmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(TypedDropStmt {
            expression: self.expression.clone_typed_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_stmt_type(&self) -> StmtType {
        StmtType::DropStmt
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for TypedDropStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(TypedDropStmt {
            expression: self.expression.clone_typed_wrapper(),
            span: self.span.clone(),
        })
    }
}
