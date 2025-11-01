//! Type checker implementation.
//!
//! This module contains the core type checking logic that validates
//! and transforms the AST. It handles:
//!
//! - Type inference and validation for expressions
//! - Variable and function scope management
//! - Type compatibility checking
//! - Function signature validation
//! - Struct type checking
//! - Memory lifetime tracking (variable drops)
//!
//! The type checker uses Environment structures to track variable
//! declarations and types across different scopes, and produces a
//! typed AST that can be safely compiled to LLVM IR.

use std::{
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
    sync::{Arc, Mutex},
};

use crate::{
    ast::{
        ast::{
            Expr, ExprType, ExprWrapper, Stmt, StmtType, StmtWrapper, Type, TypeType, TypeWrapper,
        },
        expressions::{
            AssignmentExpr, BinaryExpr, CallExpr, NumberExpr, PrefixExpr, StringExpr,
            StructInitExpr, SymbolExpr,
        },
        statements::{
            BlockStmt, BreakStmt, DropStmt, ExpressionStmt, ExternDeclStmt, FnDeclStmt, IfStmt,
            ReturnStmt, StructDeclStmt, VarDeclStmt, WhileStmt,
        },
        types::{
            ArrayType, FunctionType, LiteralType, Literals, NumberType, StructType, SymbolType,
        },
    },
    errors::errors::{Error, ErrorImpl},
    type_checker::typed_ast::{TypedBreakStmt, TypedDropStmt, TypedExternDeclStmt, TypedWhileStmt},
    Position,
};

use super::typed_ast::{
    TypedAssignmentExpr, TypedBinaryExpr, TypedBlockStmt, TypedCallExpr, TypedExpr,
    TypedExprWrapper, TypedExpressionStmt, TypedFnDeclStmt, TypedIfStmt, TypedPrefixExpr,
    TypedReturnStmt, TypedStmt, TypedStmtWrapper, TypedStructDeclStmt, TypedStructInitExpr,
    TypedSymbolExpr, TypedVarDeclStmt,
};

/// Represents a lexical scope environment for type checking.
///
/// Tracks variable and type declarations within a scope, along with
/// metadata for semantic analysis like variable lifetimes and const-ness.
#[derive(Debug)]
pub struct Environment {
    /// Maps variable names to (is_const, type, decl_pos, last_access, is_dropped)
    pub variable_lookup: HashMap<String, (bool, TypeWrapper, Position, Position, bool)>,
    /// Maps user-defined type names to their type definitions
    pub type_lookup: HashMap<String, TypeWrapper>,
    /// Unique identifier for this environment
    pub id: i32,
    /// The function this environment belongs to (if any)
    pub function: Option<FnDeclStmt>,
}

unsafe impl Send for Environment {}
unsafe impl Sync for Environment {}

impl Environment {
    /// Creates a new environment with the given ID.
    ///
    /// # Arguments
    ///
    /// * `id` - Unique identifier for this environment
    pub fn new(id: i32) -> Self {
        Environment {
            variable_lookup: HashMap::new(),
            type_lookup: HashMap::new(),
            id,
            function: None,
        }
    }

    pub fn declare_variable(
        &mut self,
        variable_name: String,
        variable_type: TypeWrapper,
        is_constant: bool,
        current_position: Position,
    ) -> Result<(), Error> {
        match self.variable_lookup.entry(variable_name.clone()) {
            Entry::Occupied(_) => Err(Error::new(
                ErrorImpl::VariableAlreadyDeclared {
                    variable: variable_name,
                },
                current_position,
            )),
            Entry::Vacant(entry) => {
                entry.insert((
                    is_constant,
                    variable_type,
                    current_position.clone(),
                    current_position,
                    false,
                ));
                Ok(())
            }
        }
    }

    pub fn get_variable(
        &self,
        variable_name: &String,
    ) -> Option<&(bool, TypeWrapper, Position, Position, bool)> {
        self.variable_lookup.get(variable_name)
    }

    pub fn update_last_access(&mut self, variable_name: &String, position: Position) {
        if let Some(var) = self.variable_lookup.get_mut(variable_name) {
            var.3 = position;
        }
    }

    pub fn get_type(&self, type_name: &String) -> Option<&TypeWrapper> {
        self.type_lookup.get(type_name)
    }

    pub fn mark_variable_as_dropped(&mut self, variable_name: &String) {
        if let Some(var) = self.variable_lookup.get_mut(variable_name) {
            var.4 = true;
        }
    }
}

#[derive(Debug)]
pub struct TypeChecker {
    pub typed_ast: Vec<TypedStmtWrapper>,
    pub built_in_types: HashMap<String, TypeWrapper>,
    pub globals: HashMap<String, TypeWrapper>,
    pub used_globals: HashMap<String, TypeWrapper>,
    pub environments: Vec<Arc<Mutex<Environment>>>,
    pub environment_lookup: HashMap<i32, usize>,
    pub environment_path: Vec<usize>,
}

impl TypeChecker {
    pub fn add_environment(&mut self, environment: Environment) -> Arc<Mutex<Environment>> {
        let environment = Arc::new(Mutex::new(environment));
        self.environments.push(Arc::clone(&environment));
        self.environment_lookup
            .insert(environment.lock().unwrap().id, self.environments.len() - 1);
        self.environment_path.push(self.environments.len() - 1);

        environment
    }

    pub fn clear_environment_path(&mut self) {
        self.environment_path = vec![];
    }

    pub fn set_current_environment(&mut self, id: i32) {
        self.environment_path
            .push(*self.environment_lookup.get(&id).unwrap());
    }

    pub fn get_current_environment(&self) -> Arc<Mutex<Environment>> {
        Arc::clone(&self.environments[self.environment_path[self.environment_path.len() - 1]])
    }

    pub fn get_nth_parent_environment(&self, nth: usize) -> Option<Arc<Mutex<Environment>>> {
        if nth >= self.environment_path.len() {
            None
        } else {
            Some(Arc::clone(
                &self.environments[self.environment_path[self.environment_path.len() - nth - 1]],
            ))
        }
    }

    pub fn fetch_variable_type(
        &mut self,
        variable: String,
        position: Position,
    ) -> Result<(bool, TypeWrapper), Error> {
        // Check if it's a reserved variable first
        if self.built_in_types.contains_key(&variable) {
            Ok((
                true,
                self.built_in_types.get(&variable).unwrap().clone_wrapper(),
            ))
        } else if self.globals.contains_key(&variable) {
            self.used_globals.insert(
                variable.clone(),
                self.globals.get(&variable).unwrap().clone_wrapper(),
            );
            Ok((true, self.globals.get(&variable).unwrap().clone_wrapper()))
        } else if self
            .get_current_environment()
            .lock()
            .unwrap()
            .get_variable(&variable)
            .is_some()
        {
            let current_env = self.get_current_environment();
            let environment = current_env.lock().unwrap();
            let var = environment.get_variable(&variable).unwrap();

            if var.4 {
                return Err(Error::new(
                    ErrorImpl::VariableAlreadyDropped { variable },
                    position,
                ));
            }

            Ok((var.0, var.1.clone_wrapper()))
        } else {
            for env_number in 0..(self.environment_path.len()) {
                if self
                    .get_nth_parent_environment(env_number)
                    .unwrap()
                    .lock()
                    .unwrap()
                    .get_variable(&variable)
                    .is_some()
                {
                    let nth_parent_env = self.get_nth_parent_environment(env_number).unwrap();
                    let nth_parent_env = nth_parent_env.lock().unwrap();
                    let var = nth_parent_env.get_variable(&variable).unwrap();

                    if var.4 {
                        return Err(Error::new(
                            ErrorImpl::VariableAlreadyDropped { variable },
                            position,
                        ));
                    }

                    return Ok((var.0, var.1.clone_wrapper()));
                }
            }
            Err(Error::new(
                ErrorImpl::UnknownType { type_: variable },
                position,
            ))
        }
    }

    pub fn convert_type(&mut self, ty: TypeWrapper) -> Result<TypeWrapper, Error> {
        match ty.get_type_type() {
            TypeType::Symbol(_) => {
                let symbol = ty.as_any().downcast_ref::<SymbolType>().unwrap();

                Ok(self
                    .fetch_variable_type(symbol.name.clone(), symbol.get_position())?
                    .1)
            }
            TypeType::Array(_) => Ok(TypeWrapper::new(ArrayType {
                underlying: self.convert_type(
                    ty.as_any()
                        .downcast_ref::<ArrayType>()
                        .unwrap()
                        .underlying
                        .clone_wrapper(),
                )?,
            })),
            _ => Ok(ty),
        }
    }
}

pub fn type_check_expr(
    type_checker: &mut TypeChecker,
    ast: ExprWrapper,
) -> Result<TypedExprWrapper, Error> {
    match ast.get_expr_type() {
        ExprType::Number => Ok(TypedExprWrapper::new(
            ast.as_any().downcast_ref::<NumberExpr>().unwrap().clone(),
        )),
        ExprType::String => Ok(TypedExprWrapper::new(
            ast.as_any().downcast_ref::<StringExpr>().unwrap().clone(),
        )),
        ExprType::Assignment => {
            let assignment = ast.as_any().downcast_ref::<AssignmentExpr>().unwrap();
            let value = type_check_expr(type_checker, assignment.value.clone_wrapper())?;
            let assignee = type_check_expr(type_checker, assignment.assignee.clone_wrapper())?;

            if TypedExpr::get_type(&value).get_type_type()
                != TypedExpr::get_type(&assignee).get_type_type()
            {
                Err(Error::new(
                    ErrorImpl::TypeMatchError {
                        expected: format!("{:?}", TypedExpr::get_type(&assignee).get_type_type()),
                        received: format!("{:?}", TypedExpr::get_type(&value).get_type_type()),
                    },
                    value.get_span().start.clone(),
                ))
            } else {
                Ok(TypedExprWrapper::new(TypedAssignmentExpr {
                    span: ast.get_span().clone(),
                    value_type: TypedExpr::get_type(&assignee).clone_wrapper(),
                    assignee,
                    operator: assignment.operator.clone(),
                    value,
                }))
            }
        }
        ExprType::Binary => {
            if ast
                .as_any()
                .downcast_ref::<BinaryExpr>()
                .unwrap()
                .operator
                .value
                == "."
            {
                // Member expression
                let binary = ast.as_any().downcast_ref::<BinaryExpr>().unwrap();
                let left = type_check_expr(type_checker, binary.left.clone_wrapper())?;
                let right = binary.right.as_any().downcast_ref::<SymbolExpr>().unwrap();
                let ty = TypedExpr::get_type(&left).get_property_type(right.value.clone());

                Ok(TypedExprWrapper::new(TypedBinaryExpr {
                    span: ast.get_span().clone(),
                    left,
                    operator: binary.operator.clone(),
                    right: TypedExprWrapper::new(TypedSymbolExpr {
                        value: right.value.clone(),
                        var_type: ty,
                        span: right.get_span().clone(),
                    }),
                }))
            } else {
                Ok(TypedExprWrapper::new(TypedBinaryExpr {
                    span: ast.get_span().clone(),
                    left: type_check_expr(
                        type_checker,
                        ast.as_any()
                            .downcast_ref::<BinaryExpr>()
                            .unwrap()
                            .left
                            .clone_wrapper(),
                    )?,
                    operator: ast
                        .as_any()
                        .downcast_ref::<BinaryExpr>()
                        .unwrap()
                        .operator
                        .clone(),
                    right: type_check_expr(
                        type_checker,
                        ast.as_any()
                            .downcast_ref::<BinaryExpr>()
                            .unwrap()
                            .right
                            .clone_wrapper(),
                    )?,
                }))
            }
        }
        ExprType::Prefix => {
            let prefix = ast.as_any().downcast_ref::<PrefixExpr>().unwrap();
            Ok(TypedExprWrapper::new(TypedPrefixExpr {
                span: ast.get_span().clone(),
                operator: prefix.operator.clone().value,
                right_expr: type_check_expr(type_checker, prefix.right_expr.clone_wrapper())?,
            }))
        }
        ExprType::Symbol => {
            let symbol = ast.as_any().downcast_ref::<SymbolExpr>().unwrap();
            let var = type_checker
                .fetch_variable_type(symbol.value.clone(), symbol.get_span().start.clone());

            type_checker
                .get_current_environment()
                .lock()
                .unwrap()
                .update_last_access(&symbol.value, symbol.get_span().end.clone());

            if var.is_err() {
                return Err(var.err().unwrap());
            }

            Ok(TypedExprWrapper::new(TypedSymbolExpr {
                span: ast.get_span().clone(),
                value: symbol.value.clone(),
                var_type: var?.1.clone_wrapper(),
            }))
        }
        ExprType::CallExpr => {
            let call = ast.as_any().downcast_ref::<CallExpr>().unwrap();
            let function_type =
                TypedExpr::get_type(&type_check_expr(type_checker, call.callee.clone_wrapper())?);
            let function = function_type
                .as_any()
                .downcast_ref::<FunctionType>()
                .unwrap();

            let mut arguments = vec![];
            for arg in call.arguments.iter() {
                arguments.push(type_check_expr(type_checker, arg.clone_wrapper())?);
            }

            if arguments.len() > function.arguments.len() && !function.is_var_args {
                return Err(Error::new(
                    ErrorImpl::UnexpectedArguments {
                        expected: function.arguments.len(),
                        received: arguments.len(),
                    },
                    arguments[function.arguments.len()].get_span().start.clone(),
                ));
            } else if arguments.len() < function.arguments.len() {
                return Err(Error::new(
                    ErrorImpl::MissingArguments {
                        expected: function.arguments.len(),
                        received: arguments.len(),
                    },
                    arguments.last().unwrap().get_span().end.clone(),
                ));
            }

            for (argument_number, argument) in function.arguments.iter().enumerate() {
                if TypedExpr::get_type(&arguments[argument_number]).get_type_type()
                    != argument.1.get_type_type()
                {
                    return Err(Error::new(
                        ErrorImpl::ArgumentTypeMatchError {
                            expected: format!("{:?}", argument.1.get_type_type()),
                            received: format!(
                                "{:?}",
                                TypedExpr::get_type(&arguments[argument_number]).get_type_type()
                            ),
                        },
                        arguments[argument_number].get_span().start.clone(),
                    ));
                }
            }

            Ok(TypedExprWrapper::new(TypedCallExpr {
                span: ast.get_span().clone(),
                callee: function.clone(),
                arguments,
            }))
        }
        ExprType::StructInit => {
            let struct_init = ast.as_any().downcast_ref::<StructInitExpr>().unwrap();
            let struct_type = type_checker
                .fetch_variable_type(struct_init.name.clone(), struct_init.span.start.clone())?
                .1
                .clone_wrapper();
            let struct_type = struct_type.as_any().downcast_ref::<StructType>().unwrap();

            let mut fields = vec![];
            for field in struct_init.fields.iter() {
                let field_type = struct_type
                    .fields
                    .iter()
                    .find(|x| x.0 == field.0)
                    .unwrap()
                    .1
                    .clone_wrapper();
                let field_expr = type_check_expr(type_checker, field.1.clone_wrapper())?;
                if TypedExpr::get_type(&field_expr).get_type_type() != field_type.get_type_type() {
                    return Err(Error::new(
                        ErrorImpl::FieldTypeMatchError {
                            expected: format!("{:?}", field_type.get_type_type()),
                            received: format!(
                                "{:?}",
                                TypedExpr::get_type(&field_expr).get_type_type()
                            ),
                        },
                        field_expr.get_span().start.clone(),
                    ));
                }
                fields.push((field.0.clone(), field_expr));
            }

            Ok(TypedExprWrapper::new(TypedStructInitExpr {
                span: ast.get_span().clone(),
                name: struct_init.name.clone(),
                fields,
            }))
        }
    }
}

pub fn type_check_stmt(
    type_checker: &mut TypeChecker,
    ast: StmtWrapper,
) -> Result<TypedStmtWrapper, Error> {
    match ast.get_stmt_type() {
        StmtType::BlockStmt => Ok(TypedStmtWrapper::new(type_check_block(
            type_checker,
            ast.clone()
                .as_any()
                .downcast_ref::<BlockStmt>()
                .unwrap()
                .clone(),
        )?)),
        StmtType::ExpressionStmt => Ok(TypedStmtWrapper::new(TypedExpressionStmt {
            expression: TypedExprWrapper::new(type_check_expr(
                type_checker,
                ast.clone()
                    .as_any()
                    .downcast_ref::<ExpressionStmt>()
                    .unwrap()
                    .expression
                    .clone_wrapper(),
            )?),
        })),
        StmtType::VarDeclStmt => {
            let var_decl_stmt = ast.as_any().downcast_ref::<VarDeclStmt>().unwrap();
            if var_decl_stmt.explicit_type.is_some() {
                if var_decl_stmt.assigned_value.is_some() {
                    if let TypeType::Symbol(_) = var_decl_stmt
                        .explicit_type
                        .as_ref()
                        .unwrap()
                        .get_type_type()
                    {
                        let symbol_type = var_decl_stmt
                            .explicit_type
                            .as_ref()
                            .unwrap()
                            .as_any()
                            .downcast_ref::<SymbolType>()
                            .unwrap();
                        let explicit_type = type_checker
                            .fetch_variable_type(
                                symbol_type.name.clone(),
                                symbol_type.get_position(),
                            )?
                            .1;
                        let actual_type = type_check_expr(
                            type_checker,
                            var_decl_stmt
                                .assigned_value
                                .as_ref()
                                .unwrap()
                                .clone_wrapper(),
                        )?;

                        if TypedExpr::get_type(&actual_type).get_type_type()
                            != explicit_type.get_type_type()
                            && !explicit_type.is_compatible_with(
                                &TypedExpr::get_type(&actual_type).get_type_type(),
                            )
                            && explicit_type.get_type_type() != TypeType::Literal(Literals::String)
                        {
                            println!("1");
                            return Err(Error::new(
                                ErrorImpl::TypeMatchError {
                                    expected: format!("{:?}", explicit_type.get_type_type()),
                                    received: format!(
                                        "{:?}",
                                        TypedExpr::get_type(&actual_type).get_type_type()
                                    ),
                                },
                                actual_type.get_span().start.clone(),
                            ));
                        }
                        type_checker
                            .get_current_environment()
                            .lock()
                            .unwrap()
                            .declare_variable(
                                var_decl_stmt.identifier.clone(),
                                explicit_type.clone_wrapper(),
                                var_decl_stmt.is_constant,
                                var_decl_stmt.get_span().start.clone(),
                            )?;
                        Ok(TypedStmtWrapper::new(TypedVarDeclStmt {
                            identifier: var_decl_stmt.identifier.clone(),
                            is_constant: var_decl_stmt.is_constant,
                            assigned_value: Some(actual_type),
                            var_type: explicit_type,
                            span: var_decl_stmt.get_span().clone(),
                        }))
                    } else {
                        // Only support symbol types for now
                        todo!();
                    }
                } else {
                    todo!();
                    // Only support symbol types for now
                }
            } else if var_decl_stmt.assigned_value.is_none() {
                Err(Error::new(
                    ErrorImpl::ExpectedExplicitValue,
                    var_decl_stmt.get_span().end.clone(),
                ))
            } else {
                let expr = type_check_expr(
                    type_checker,
                    var_decl_stmt
                        .assigned_value
                        .as_ref()
                        .unwrap()
                        .clone_wrapper(),
                )?;
                type_checker
                    .get_current_environment()
                    .lock()
                    .unwrap()
                    .declare_variable(
                        var_decl_stmt.identifier.clone(),
                        TypedExpr::get_type(&expr),
                        var_decl_stmt.is_constant,
                        var_decl_stmt.get_span().start.clone(),
                    )?;
                Ok(TypedStmtWrapper::new(TypedVarDeclStmt {
                    identifier: var_decl_stmt.identifier.clone(),
                    is_constant: var_decl_stmt.is_constant,
                    var_type: TypedExpr::get_type(&expr),
                    assigned_value: Some(expr),
                    span: var_decl_stmt.get_span().clone(),
                }))
            }
        }
        StmtType::IfStmt => {
            let if_stmt = ast.as_any().downcast_ref::<IfStmt>().unwrap();

            Ok(TypedStmtWrapper::new(TypedIfStmt {
                span: ast.get_span().clone(),
                condition: type_check_expr(type_checker, if_stmt.condition.clone_wrapper())?,
                then_body: type_check_stmt(type_checker, if_stmt.then_body.clone())?,
                else_body: if if_stmt.else_body.is_some() {
                    Some(type_check_stmt(
                        type_checker,
                        if_stmt.else_body.clone().unwrap(),
                    )?)
                } else {
                    None
                },
            }))
        }
        StmtType::FnDeclStmt => {
            let fn_decl_stmt = ast.as_any().downcast_ref::<FnDeclStmt>().unwrap();

            if type_checker
                .get_current_environment()
                .lock()
                .unwrap()
                .get_variable(&fn_decl_stmt.identifier)
                .is_some()
            {
                return Err(Error::new(
                    ErrorImpl::FunctionAlreadyDeclared {
                        function: fn_decl_stmt.identifier.clone(),
                    },
                    fn_decl_stmt.get_span().start.clone(),
                ));
            }

            type_checker.add_environment(Environment {
                variable_lookup: HashMap::new(),
                type_lookup: HashMap::new(),
                id: fn_decl_stmt.body.id,
                function: Some(fn_decl_stmt.clone()),
            });

            let mut arguments = vec![];
            for arg in fn_decl_stmt.parameters.iter() {
                let ty = type_checker.convert_type(arg.1.clone_wrapper())?;
                arguments.push((arg.0.clone(), ty.clone_wrapper()));
                type_checker
                    .get_current_environment()
                    .lock()
                    .unwrap()
                    .declare_variable(arg.0.clone(), ty, false, Position::null())?;
            }

            let body = Rc::new(type_check_block(type_checker, fn_decl_stmt.body.clone())?);

            type_checker
                .get_current_environment()
                .lock()
                .unwrap()
                .declare_variable(
                    fn_decl_stmt.identifier.clone(),
                    TypeWrapper::new(FunctionType {
                        name: fn_decl_stmt.identifier.clone(),
                        is_native: false,
                        body: Some(Rc::clone(&body)),
                        return_type: type_checker
                            .convert_type(fn_decl_stmt.return_type.clone_wrapper())?
                            .clone_wrapper(),
                        arguments: arguments
                            .iter()
                            .map(|x| (x.0.clone(), x.1.clone_wrapper()))
                            .collect::<Vec<(String, TypeWrapper)>>(),
                        is_var_args: false,
                        should_export: true,
                    }),
                    false,
                    fn_decl_stmt.span.start.clone(),
                )?;

            Ok(TypedStmtWrapper::new(TypedFnDeclStmt {
                span: ast.get_span().clone(),
                identifier: fn_decl_stmt.identifier.clone(),
                parameters: arguments,
                return_type: type_checker
                    .convert_type(fn_decl_stmt.return_type.clone_wrapper())?
                    .clone_wrapper(),
                body: (*body).clone(),
            }))
        }
        StmtType::ExternDeclStmt => {
            let extern_decl_stmt = ast.as_any().downcast_ref::<ExternDeclStmt>().unwrap();

            if type_checker
                .get_current_environment()
                .lock()
                .unwrap()
                .get_variable(&extern_decl_stmt.identifier)
                .is_some()
            {
                return Err(Error::new(
                    ErrorImpl::FunctionAlreadyDeclared {
                        function: extern_decl_stmt.identifier.clone(),
                    },
                    extern_decl_stmt.get_span().start.clone(),
                ));
            }

            let mut arguments = vec![];
            for arg in extern_decl_stmt.parameters.iter() {
                let ty = type_checker.convert_type(arg.1.clone_wrapper())?;
                arguments.push((arg.0.clone(), ty.clone_wrapper()));
            }

            type_checker
                .get_current_environment()
                .lock()
                .unwrap()
                .declare_variable(
                    extern_decl_stmt.identifier.clone(),
                    TypeWrapper::new(FunctionType {
                        name: extern_decl_stmt.identifier.clone(),
                        is_native: true,
                        body: None,
                        return_type: type_checker
                            .convert_type(extern_decl_stmt.return_type.clone_wrapper())?
                            .clone_wrapper(),
                        arguments: arguments
                            .iter()
                            .map(|x| (x.0.clone(), x.1.clone_wrapper()))
                            .collect::<Vec<(String, TypeWrapper)>>(),
                        is_var_args: extern_decl_stmt.is_variadic,
                        should_export: false,
                    }),
                    false,
                    extern_decl_stmt.span.start.clone(),
                )?;

            Ok(TypedStmtWrapper::new(TypedExternDeclStmt {
                span: ast.get_span().clone(),
                identifier: extern_decl_stmt.identifier.clone(),
                parameters: arguments,
                is_variadic: extern_decl_stmt.is_variadic,
                return_type: type_checker
                    .convert_type(extern_decl_stmt.return_type.clone_wrapper())?
                    .clone_wrapper(),
            }))
        }
        StmtType::ReturnStmt => {
            let return_stmt = ast.as_any().downcast_ref::<ReturnStmt>().unwrap();
            let current_env = type_checker.get_current_environment();
            let environment = current_env.lock().unwrap();

            if environment.function.is_none() {
                // type_checker.add_error(Box::new(TypeCheckerException {
                //     trace: Trace {
                //         parent: None,
                //         position: crate::Position(0, Rc::new(String::from("Null")))
                //     },
                //     message: String::from(format!("Return statement outside of function")),
                //     position: crate::Position(0, Rc::new(String::from("Null")))
                // }));
                // return Err(Box::new(TypeCheckerException {
                //     trace: Trace {
                //         parent: None,
                //         position: crate::Position(0, Rc::new(String::from("Null")))
                //     },
                //     message: String::from(format!("Return statement outside of function")),
                //     position: crate::Position(0, Rc::new(String::from("Null")))
                // }));
                todo!()
            } else {
                let return_type = environment
                    .function
                    .as_ref()
                    .unwrap()
                    .return_type
                    .clone_wrapper()
                    .get_type_type();
                drop(environment); // Stop the lock
                if let TypeType::Symbol(_) = return_type {
                    let environment = type_checker.get_current_environment();
                    let environment = environment.lock().unwrap();
                    let function = environment.function.as_ref().unwrap().clone();

                    drop(environment); // Stop the lock

                    let return_symbol = function
                        .return_type
                        .as_any()
                        .downcast_ref::<SymbolType>()
                        .unwrap();
                    let return_type = type_checker
                        .fetch_variable_type(
                            return_symbol.name.clone(),
                            return_symbol.get_position(),
                        )
                        .unwrap()
                        .1
                        .clone_wrapper();
                    if return_stmt.value.is_some() {
                        let value = type_check_expr(
                            type_checker,
                            return_stmt.value.as_ref().unwrap().clone_wrapper(),
                        )?;
                        if TypedExpr::get_type(&value).get_type_type()
                            != return_type.get_type_type()
                        {
                            // type_checker.add_error(Box::new(TypeCheckerException {
                            //     trace: Trace {
                            //         parent: None,
                            //         position: crate::Position(0, Rc::new(String::from("Null")))
                            //     },
                            //     message: String::from(format!("Return type does not match")),
                            //     position: crate::Position(0, Rc::new(String::from("Null")))
                            // }));
                            // return Err(Box::new(TypeCheckerException {
                            //     trace: Trace {
                            //         parent: None,
                            //         position: crate::Position(0, Rc::new(String::from("Null")))
                            //     },
                            //     message: String::from(format!("Return type does not match")),
                            //     position: crate::Position(0, Rc::new(String::from("Null")))
                            // }));
                            todo!()
                        }

                        Ok(TypedStmtWrapper::new(TypedReturnStmt {
                            span: ast.get_span().clone(),
                            value: Some(value),
                        }))
                    } else {
                        todo!()
                    }
                } else {
                    // type_checker.add_error(Box::new(TypeCheckerException {
                    //     trace: Trace {
                    //         parent: None,
                    //         position: crate::Position(0, Rc::new(String::from("Null")))
                    //     },
                    //     message: String::from(format!("Expected return value")),
                    //     position: crate::Position(0, Rc::new(String::from("Null")))
                    // }));
                    // return Err(Box::new(TypeCheckerException {
                    //     trace: Trace {
                    //         parent: None,
                    //         position: crate::Position(0, Rc::new(String::from("Null")))
                    //     },
                    //     message: String::from(format!("Expected return value")),
                    //     position: crate::Position(0, Rc::new(String::from("Null")))
                    // }));
                    todo!()
                }
            }
        }
        StmtType::StructDeclStmt => {
            let struct_decl_stmt = ast.as_any().downcast_ref::<StructDeclStmt>().unwrap();
            let mut fields = vec![];

            for field in struct_decl_stmt.fields.iter() {
                let ty = type_checker.convert_type(field.1.clone_wrapper())?;
                fields.push((field.0.clone(), ty.clone_wrapper()));
                type_checker
                    .get_current_environment()
                    .lock()
                    .unwrap()
                    .declare_variable(field.0.clone(), ty, false, Position::null())?;
            }

            type_checker
                .get_current_environment()
                .lock()
                .unwrap()
                .declare_variable(
                    struct_decl_stmt.name.clone(),
                    TypeWrapper::new(StructType {
                        name: struct_decl_stmt.name.clone(),
                        fields: fields
                            .iter()
                            .map(|x| (x.0.clone(), x.1.clone_wrapper()))
                            .collect::<Vec<(String, TypeWrapper)>>(),
                    }),
                    false,
                    struct_decl_stmt.span.start.clone(),
                )?;

            Ok(TypedStmtWrapper::new(TypedStructDeclStmt {
                span: ast.get_span().clone(),
                name: struct_decl_stmt.name.clone(),
                fields,
            }))
        }
        StmtType::BreakStmt => {
            let break_stmt = ast.as_any().downcast_ref::<BreakStmt>().unwrap();
            Ok(TypedStmtWrapper::new(TypedBreakStmt {
                span: break_stmt.span.clone(),
            }))
        }
        StmtType::WhileStmt => {
            let while_stmt = ast.as_any().downcast_ref::<WhileStmt>().unwrap();
            Ok(TypedStmtWrapper::new(TypedWhileStmt {
                condition: type_check_expr(type_checker, while_stmt.condition.clone_wrapper())?,
                body: type_check_stmt(type_checker, while_stmt.body.clone())?,
                span: while_stmt.span.clone(),
            }))
        }
        StmtType::DropStmt => {
            let drop_stmt = ast.as_any().downcast_ref::<DropStmt>().unwrap();

            let expression = type_check_expr(type_checker, drop_stmt.expression.clone_wrapper())?;

            if drop_stmt.expression.get_expr_type() == ExprType::Symbol {
                let symbol = drop_stmt
                    .expression
                    .as_any()
                    .downcast_ref::<SymbolExpr>()
                    .unwrap();
                type_checker
                    .get_current_environment()
                    .lock()
                    .unwrap()
                    .mark_variable_as_dropped(&symbol.value);
            } else {
                todo!()
            }

            Ok(TypedStmtWrapper::new(TypedDropStmt {
                expression,
                span: drop_stmt.span.clone(),
            }))
        }
        _ => {
            todo!()
        }
    }
}

#[allow(unreachable_code)]
pub fn type_check_block(
    type_checker: &mut TypeChecker,
    mut ast: BlockStmt,
) -> Result<TypedBlockStmt, Error> {
    if !type_checker.environment_lookup.contains_key(&ast.id) {
        type_checker.add_environment(Environment {
            variable_lookup: HashMap::new(),
            type_lookup: HashMap::new(),
            id: ast.id,
            function: None,
        });
    }

    let mut typed_ast = TypedBlockStmt {
        id: ast.id,
        body: vec![],
        span: ast.span.clone(),
    };
    for stmt in ast.iter_mut() {
        let a = type_check_stmt(type_checker, stmt.clone())?;
        typed_ast.body.push(a);
    }

    type_checker.environment_path.pop();
    Ok(typed_ast)
}

pub fn type_check(
    ast: BlockStmt,
    available_functions: Option<&Vec<(String, TypeWrapper)>>,
    is_std_lib: bool,
) -> (TypeChecker, Option<Error>) {
    // Loop through the body, keeping track of different types
    // Store current scope
    let mut type_checker = TypeChecker {
        typed_ast: vec![],
        globals: HashMap::new(),
        used_globals: HashMap::new(),
        built_in_types: HashMap::new(),
        environments: vec![],
        environment_lookup: HashMap::new(),
        environment_path: vec![],
    };

    if let Some(functions) = available_functions {
        for function in functions {
            type_checker
                .globals
                .insert(function.0.clone(), function.1.clone_wrapper());
        }
    }

    // Built in types
    type_checker.built_in_types.insert(
        String::from("string"),
        TypeWrapper::new(LiteralType {
            literal: Literals::String,
        }),
    );
    type_checker.built_in_types.insert(
        String::from("i32"),
        TypeWrapper::new(LiteralType {
            literal: Literals::Number(NumberType::Int32),
        }),
    );
    type_checker.built_in_types.insert(
        String::from("i64"),
        TypeWrapper::new(LiteralType {
            literal: Literals::Number(NumberType::Int64),
        }),
    );
    type_checker.built_in_types.insert(
        String::from("i8"),
        TypeWrapper::new(LiteralType {
            literal: Literals::Number(NumberType::Int8),
        }),
    );
    type_checker.built_in_types.insert(
        String::from("i16"),
        TypeWrapper::new(LiteralType {
            literal: Literals::Number(NumberType::Int16),
        }),
    );
    type_checker.built_in_types.insert(
        String::from("bool"),
        TypeWrapper::new(LiteralType {
            literal: Literals::Boolean,
        }),
    );

    type_checker.globals.insert(
        String::from("true"),
        TypeWrapper::new(LiteralType {
            literal: Literals::Boolean,
        }),
    );
    type_checker.globals.insert(
        String::from("false"),
        TypeWrapper::new(LiteralType {
            literal: Literals::Boolean,
        }),
    );

    // Built internal std types
    if is_std_lib {
        type_checker.built_in_types.insert(
            String::from("__i8ptr__"),
            TypeWrapper::new(LiteralType {
                literal: Literals::InternalI8Pointer,
            }),
        );
    }

    // Built in functions
    type_checker.globals.insert(
        String::from("strcmp"),
        TypeWrapper::new(FunctionType {
            name: String::from("strcmp"),
            is_native: true,
            body: None,
            return_type: TypeWrapper::new(LiteralType {
                literal: Literals::Number(NumberType::Int32),
            }),
            arguments: vec![
                (
                    String::from("str1"),
                    TypeWrapper::new(LiteralType {
                        literal: Literals::String,
                    }),
                ),
                (
                    String::from("str2"),
                    TypeWrapper::new(LiteralType {
                        literal: Literals::String,
                    }),
                ),
            ],
            is_var_args: false,
            should_export: false,
        }),
    );
    type_checker.globals.insert(
        String::from("strlen"),
        TypeWrapper::new(FunctionType {
            name: String::from("strlen"),
            is_native: true,
            body: None,
            return_type: TypeWrapper::new(LiteralType {
                literal: Literals::Number(NumberType::Int32),
            }),
            arguments: vec![(
                String::from("str"),
                TypeWrapper::new(LiteralType {
                    literal: Literals::String,
                }),
            )],
            is_var_args: false,
            should_export: false,
        }),
    );

    let block = type_check_block(&mut type_checker, ast);

    if let Err(err) = block {
        (type_checker, Some(err))
    } else if let Ok(typed_block) = block {
        type_checker
            .typed_ast
            .push(typed_block.clone_typed_wrapper());
        (type_checker, None)
    } else {
        unreachable!()
    }
}
