use std::{any::Any, slice::{Iter, IterMut}};

use crate::{type_checker::typed_ast::{TypedStmt, TypedStmtWrapper}, Span};

use super::ast::{Expr, ExprWrapper, Stmt, StmtType, StmtWrapper, Type, TypeWrapper};

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub body: Vec<StmtWrapper>,
    pub id: i32,
    pub span: Span
}

impl BlockStmt {
    pub fn iter(&self) -> Iter<'_, StmtWrapper> {
        self.body.iter()
    }
    pub fn iter_mut(&mut self) -> IterMut<'_, StmtWrapper> {
        self.body.iter_mut()
    }
}

impl Stmt for BlockStmt {
    fn get_stmt_type(&self) -> StmtType {
        StmtType::BlockStmt
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(self.clone())
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}


#[derive(Debug)]
pub struct ExpressionStmt {
    pub expression: ExprWrapper,
    pub span: Span
}

impl Stmt for ExpressionStmt {
    fn get_stmt_type(&self) -> StmtType {
        StmtType::ExpressionStmt
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        self.expression.into_cloned_stmt_wrapper()
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}


#[derive(Debug)]
pub struct VarDeclStmt {
    pub identifier: String,
    pub is_constant: bool,
    pub assigned_value: Option<ExprWrapper>,
    pub explicit_type: Option<TypeWrapper>,
    pub span: Span
}

impl Stmt for VarDeclStmt {
    fn get_stmt_type(&self) -> StmtType {
        StmtType::VarDeclStmt
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(
            VarDeclStmt {
                identifier: self.identifier.clone(),
                is_constant: self.is_constant,
                assigned_value: if self.assigned_value.is_none() { None } else { Some(self.assigned_value.as_ref().unwrap().clone_wrapper()) },
                explicit_type: if self.explicit_type.is_none() { None } else { Some(self.explicit_type.as_ref().unwrap().clone_wrapper()) },
                span: self.span.clone()
            }
        )
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub identifier: String,
    pub from: Option<String>,
    pub span: Span
}

impl Stmt for ImportStmt {
    fn get_stmt_type(&self) -> StmtType {
        StmtType::ImportStmt
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(self.clone())
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl TypedStmt for ImportStmt {
    fn clone_typed_wrapper(&self) -> TypedStmtWrapper {
        TypedStmtWrapper::new(self.clone())
    }
}

#[derive(Debug)]
pub struct IfStmt {
    pub condition: ExprWrapper,
    pub then_body: StmtWrapper,
    pub else_body: Option<StmtWrapper>,
    pub span: Span
}

impl Stmt for IfStmt {
    fn get_stmt_type(&self) -> StmtType {
        StmtType::IfStmt
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(IfStmt {
            condition: self.condition.clone_wrapper(),
            then_body: self.then_body.clone(),
            else_body: if self.else_body.is_none() { None } else { Some(self.else_body.as_ref().unwrap().clone()) },
            span: self.span.clone()
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub struct FnDeclStmt {
    pub identifier: String,
    pub parameters: Vec<(String, TypeWrapper)>,
    pub return_type: TypeWrapper,
    pub body: BlockStmt,
    pub span: Span
}

impl Stmt for FnDeclStmt {
    fn get_stmt_type(&self) -> StmtType {
        StmtType::FnDeclStmt
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(FnDeclStmt {
            identifier: self.identifier.clone(),
            parameters: self.parameters.iter().map(|(id, ty)| (id.clone(), ty.clone_wrapper())).collect(),
            return_type: self.return_type.clone_wrapper(),
            body: self.body.clone(),
            span: self.span.clone()
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

impl Clone for FnDeclStmt {
    fn clone(&self) -> Self {
        FnDeclStmt {
            identifier: self.identifier.clone(),
            parameters: self.parameters.iter().map(|(id, ty)| (id.clone(), ty.clone_wrapper())).collect(),
            return_type: self.return_type.clone_wrapper(),
            body: self.body.clone(),
            span: self.span.clone()
        }
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub value: Option<ExprWrapper>,
    pub span: Span
}

impl Stmt for ReturnStmt {
    fn get_stmt_type(&self) -> StmtType {
        StmtType::ReturnStmt
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(ReturnStmt {
            value: if self.value.is_none() { None } else { Some(self.value.as_ref().unwrap().clone_wrapper()) },
            span: self.span.clone()
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub struct StructDeclStmt {
    pub name: String,
    pub fields: Vec<(String, TypeWrapper)>,
    pub span: Span
}

impl Stmt for StructDeclStmt {
    fn get_stmt_type(&self) -> StmtType {
        StmtType::StructDeclStmt
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_wrapper(&self) -> StmtWrapper {
        StmtWrapper::new(StructDeclStmt {
            name: self.name.clone(),
            fields: self.fields.iter().map(|(id, ty)| (id.clone(), ty.clone_wrapper())).collect(),
            span: self.span.clone()
        })
    }
    fn get_span(&self) -> &Span {
        &self.span
    }
}