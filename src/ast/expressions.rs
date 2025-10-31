use std::any::Any;

use crate::{
    ast::types::NumberType,
    lexer::tokens::Token,
    type_checker::{
        type_checker::TypeChecker,
        typed_ast::{TypedExpr, TypedExprWrapper},
    },
    Span,
};

use super::{
    ast::{Expr, ExprType, ExprWrapper, Type, TypeWrapper},
    types::{FunctionType, LiteralType, Literals},
};

// LITERALS

/// Number Expression
/// Represents a numeric literal in the AST.
#[derive(Debug, Clone)]
pub struct NumberExpr {
    pub value: f64,
    pub span: Span,
}

impl Expr for NumberExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Number
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        TypeWrapper::new(LiteralType {
            literal: Literals::Number(NumberType::Int32),
        })
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(self.clone())
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

impl TypedExpr for NumberExpr {
    fn get_type(&self) -> TypeWrapper {
        TypeWrapper::new(LiteralType {
            literal: Literals::Number(NumberType::Int32),
        })
    }
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper::new(NumberExpr {
            value: self.value,
            span: self.span.clone(),
        })
    }
}

/// String Expression
/// Represents a string literal in the AST.
#[derive(Debug, Clone)]
pub struct StringExpr {
    pub value: String,
    pub span: Span,
}

impl Expr for StringExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::String
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        TypeWrapper::new(LiteralType {
            literal: Literals::String,
        })
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(self.clone())
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

impl TypedExpr for StringExpr {
    fn get_type(&self) -> TypeWrapper {
        TypeWrapper::new(LiteralType {
            literal: Literals::String,
        })
    }
    fn clone_typed_wrapper(&self) -> TypedExprWrapper {
        TypedExprWrapper::new(StringExpr {
            value: self.value.clone(),
            span: self.span.clone(),
        })
    }
}

/// Symbol Expression
/// Represents an identifier in the AST. This includes functions.
#[derive(Debug, Clone)]
pub struct SymbolExpr {
    pub value: String,
    pub span: Span,
}

impl Expr for SymbolExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Symbol
    }
    fn get_type(&self, type_checker: &mut TypeChecker) -> TypeWrapper {
        // Fetch value from type_checker
        type_checker
            .fetch_variable_type(self.value.clone(), self.span.start.clone())
            .expect("Variable not defined")
            .1
            .clone_wrapper()
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(self.clone())
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

// COMPLEX

/// Binary Expression
/// Represents a binary operation between two expressions in the AST.
///
/// This includes member expressions (`a.b`) as well as arithmetic operations (`a + b`).
#[derive(Debug)]
pub struct BinaryExpr {
    pub left: ExprWrapper,
    pub operator: Token,
    pub right: ExprWrapper,
    pub span: Span,
}

impl Expr for BinaryExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Binary
    }
    fn get_type(&self, type_checker: &mut TypeChecker) -> TypeWrapper {
        self.left.get_type(type_checker)
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(BinaryExpr {
            left: self.left.clone_wrapper(),
            operator: self.operator.clone(),
            right: self.right.clone_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

/// Prefix Expression
/// Represents a prefix operation on an expression in the AST.
#[derive(Debug)]
pub struct PrefixExpr {
    pub operator: Token,
    pub right_expr: ExprWrapper,
    pub span: Span,
}

impl Expr for PrefixExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Prefix
    }
    fn get_type(&self, type_checker: &mut TypeChecker) -> TypeWrapper {
        self.right_expr.get_type(type_checker)
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(PrefixExpr {
            operator: self.operator.clone(),
            right_expr: self.right_expr.clone_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

/// Assignment Expression
/// Represents an assignment operation in the AST.
#[derive(Debug)]
pub struct AssignmentExpr {
    pub assignee: ExprWrapper,
    pub operator: Token,
    pub value: ExprWrapper,
    pub span: Span,
}

impl Expr for AssignmentExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::Assignment
    }
    fn get_type(&self, type_checker: &mut TypeChecker) -> TypeWrapper {
        self.value.get_type(type_checker)
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        ExprWrapper::new(AssignmentExpr {
            assignee: self.assignee.clone_wrapper(),
            operator: self.operator.clone(),
            value: self.value.clone_wrapper(),
            span: self.span.clone(),
        })
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

/// Call Expression
/// Represents a function call in the AST.
#[derive(Debug)]
pub struct CallExpr {
    pub callee: ExprWrapper,
    pub arguments: Vec<ExprWrapper>,
    pub span: Span,
}

impl Expr for CallExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        let cloned_args = self
            .arguments
            .iter()
            .map(|x| x.clone_wrapper())
            .collect::<Vec<ExprWrapper>>();

        ExprWrapper::new(CallExpr {
            callee: self.callee.clone_wrapper(),
            arguments: cloned_args,
            span: self.span.clone(),
        })
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::CallExpr
    }
    fn get_type(&self, _type_checker: &mut TypeChecker) -> TypeWrapper {
        self.callee
            .as_any()
            .downcast_ref::<FunctionType>()
            .expect("Expected a function when calling")
            .return_type
            .clone_wrapper()
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}

/// Struct Initialization Expression
/// Represents the initialization of a struct in the AST.
#[derive(Debug)]
pub struct StructInitExpr {
    pub name: String,
    pub fields: Vec<(String, ExprWrapper)>,
    pub span: Span,
}

impl Expr for StructInitExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> ExprWrapper {
        let cloned_fields = self
            .fields
            .iter()
            .map(|(id, expr)| (id.clone(), expr.clone_wrapper()))
            .collect::<Vec<(String, ExprWrapper)>>();

        ExprWrapper::new(StructInitExpr {
            name: self.name.clone(),
            fields: cloned_fields,
            span: self.span.clone(),
        })
    }
    fn get_expr_type(&self) -> ExprType {
        ExprType::StructInit
    }
    fn get_type(&self, type_checker: &mut TypeChecker) -> TypeWrapper {
        type_checker
            .fetch_variable_type(self.name.clone(), self.span.start.clone())
            .expect("Variable not defined")
            .1
            .clone_wrapper()
    }
    fn get_span(&self) -> &crate::Span {
        &self.span
    }
}
