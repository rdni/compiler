//! Expression code generation module.
//!
//! This module handles the generation of LLVM IR for all expression types,
//! including literals, binary operations, function calls, variable access,
//! assignments, and struct initialization.

use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum},
    IntPredicate,
};

use crate::{
    ast::{
        ast::{Expr, ExprType, Type, TypeType, TypeWrapper},
        expressions::{NumberExpr, StringExpr},
        types::{self, Literals, NumberType, StructType},
    },
    lexer::tokens::{Token, TokenKind},
    type_checker::typed_ast::{
        TypedAssignmentExpr, TypedBinaryExpr, TypedCallExpr, TypedExpr, TypedExprWrapper,
        TypedStructInitExpr, TypedSymbolExpr,
    },
};

use super::compiler::Compiler;

/// Generates LLVM IR for the given expression.
///
/// This function handles all expression types including:
/// - String literals (as global string pointers)
/// - Number literals (with optional type coercion)
/// - Function calls
/// - Symbol/variable references (including boolean literals)
/// - Binary operations (arithmetic, comparison, string operations, struct field access)
/// - Assignment expressions
/// - Struct initialization
///
/// # Arguments
///
/// * `compiler` - Reference to the compiler instance
/// * `expression` - The typed expression wrapper to compile
/// * `expected_type` - Optional expected type for type coercion (used for numbers)
///
/// # Returns
///
/// Returns the LLVM BasicValueEnum representing the computed expression value.
///
/// # Panics
///
/// Panics if:
/// - A referenced function or variable is not found (indicates a type checking bug)
/// - Type mismatch occurs in binary operations (indicates a type checking bug)
/// - An invalid operator is used
/// - An unhandled expression type is encountered
pub fn gen_expression<'a>(
    compiler: &Compiler<'a>,
    expression: &TypedExprWrapper,
    expected_type: Option<TypeType>,
) -> BasicValueEnum<'a> {
    match expression.get_expr_type() {
        ExprType::String => compiler
            .builder
            .build_global_string_ptr(
                &expression
                    .as_any()
                    .downcast_ref::<StringExpr>()
                    .unwrap()
                    .value,
                "",
            )
            .unwrap()
            .as_pointer_value()
            .into(),
        ExprType::Number => {
            if let Some(expected) = expected_type {
                // Check if an expected type is provided
                match expected {
                    TypeType::Literal(Literals::Number(NumberType::Int8)) => {
                        compiler.context.i8_type()
                    }
                    TypeType::Literal(Literals::Number(NumberType::Int16)) => {
                        compiler.context.i16_type()
                    }
                    TypeType::Literal(Literals::Number(NumberType::Int32)) => {
                        compiler.context.i32_type()
                    }
                    TypeType::Literal(Literals::Number(NumberType::Int64)) => {
                        compiler.context.i64_type()
                    }
                    _ => todo!(),
                }
                .const_int(
                    expression
                        .as_any()
                        .downcast_ref::<NumberExpr>()
                        .unwrap()
                        .value as u64,
                    false,
                )
                .into()
            } else {
                compiler
                    .context
                    .i32_type()
                    .const_int(
                        expression
                            .as_any()
                            .downcast_ref::<NumberExpr>()
                            .unwrap()
                            .value as u64,
                        false,
                    )
                    .into()
            }
        }
        ExprType::CallExpr => {
            let call_expr = expression.as_any().downcast_ref::<TypedCallExpr>().unwrap();
            let symbol = call_expr
                .callee
                .as_any()
                .downcast_ref::<types::FunctionType>()
                .unwrap()
                .name
                .clone();

            // Get the function from the module
            // This should never fail unless there's a type checking/compilation bug
            let function = compiler.module.get_function(&symbol).unwrap_or_else(|| {
                panic!("Function {} not found", symbol);
            });

            let args: Vec<BasicMetadataValueEnum<'a>> = call_expr
                .arguments
                .iter()
                .map(|arg| gen_expression(compiler, arg, None).into())
                .collect();

            compiler
                .builder
                .build_call(function, &args[..], "")
                .unwrap()
                .try_as_basic_value()
                .left()
                .unwrap_or(compiler.context.i32_type().const_zero().into())
        }
        ExprType::Symbol => {
            let symbol = expression
                .as_any()
                .downcast_ref::<TypedSymbolExpr>()
                .unwrap();

            // Handle boolean literals
            if symbol.value == "true" {
                return compiler.context.bool_type().const_int(1, false).into();
            } else if symbol.value == "false" {
                return compiler.context.bool_type().const_int(0, false).into();
            }

            // Load the variable from the named allocas
            // This should never fail unless there's a type checking/compilation bug
            let alloca = compiler
                .named_allocas
                .get(&symbol.value)
                .unwrap_or_else(|| panic!("Variable {:?} not found", symbol.value));

            let value = compiler.builder.build_load(*alloca, &symbol.value).unwrap();

            value
        }
        ExprType::Binary => {
            let binary_expr = expression
                .as_any()
                .downcast_ref::<TypedBinaryExpr>()
                .unwrap();

            if binary_expr.operator.value == "." {
                // Member expr
                let left = gen_expression(compiler, &binary_expr.left, None);
                let right = binary_expr
                    .right
                    .as_any()
                    .downcast_ref::<TypedSymbolExpr>()
                    .unwrap();

                let property_name = right.value.clone();

                if let Some(struct_type) = TypedExpr::get_type(&binary_expr.left)
                    .as_any()
                    .downcast_ref::<StructType>()
                {
                    let index = struct_type
                        .fields
                        .iter()
                        .position(|field| field.0 == property_name)
                        .unwrap() as u32;
                    let field_ptr = compiler
                        .builder
                        .build_struct_gep(left.into_pointer_value(), index, "")
                        .unwrap();
                    return compiler
                        .builder
                        .build_load(field_ptr, &property_name)
                        .unwrap();
                } else {
                    todo!();
                }
            }

            let left = gen_expression(compiler, &binary_expr.left, expected_type.clone());
            let right = gen_expression(compiler, &binary_expr.right, expected_type);

            // Ensure both sides have the same type
            // This should never fail unless there's a type checking/compilation bug
            if left.get_type() != right.get_type() {
                println!("Full expression: {:?}", binary_expr);
                println!("Left type: {:?}", left.get_type());
                println!("Right type: {:?}", right.get_type());
                panic!("Type mismatch");
            }

            build_binary_operation(
                compiler,
                &binary_expr.operator,
                left,
                right,
                TypedExpr::get_type(&binary_expr.left),
            )
        }
        ExprType::Assignment => {
            let assignment_expr = expression
                .as_any()
                .downcast_ref::<TypedAssignmentExpr>()
                .unwrap();

            // Check if the compiler should change type checking based on the stdlib
            let mut value = if TypedExpr::get_type(&assignment_expr.value).get_type_type()
                != assignment_expr.value_type.get_type_type()
            {
                gen_expression(
                    compiler,
                    &assignment_expr.value,
                    Some(assignment_expr.value_type.get_type_type()),
                )
            } else {
                gen_expression(compiler, &assignment_expr.value, None)
            };

            if assignment_expr
                .assignee
                .as_any()
                .downcast_ref::<TypedBinaryExpr>()
                .is_some()
            {
                // Member expression

                let binary_expr = assignment_expr
                    .assignee
                    .as_any()
                    .downcast_ref::<TypedBinaryExpr>()
                    .unwrap();
                let struct_value = gen_expression(compiler, &binary_expr.left, None);
                let field_name = binary_expr
                    .right
                    .as_any()
                    .downcast_ref::<TypedSymbolExpr>()
                    .unwrap()
                    .value
                    .clone();

                let struct_type = TypedExpr::get_type(&binary_expr.left);
                let struct_type = struct_type.as_any().downcast_ref::<StructType>().unwrap();
                let index = struct_type
                    .fields
                    .iter()
                    .position(|field| field.0 == field_name)
                    .unwrap() as u32;
                let field_ptr = compiler
                    .builder
                    .build_struct_gep(struct_value.into_pointer_value(), index, "")
                    .unwrap();

                if assignment_expr.operator.kind != TokenKind::Assignment {
                    let current_value = compiler.builder.build_load(field_ptr, "").unwrap();
                    let new_value = build_binary_operation(
                        compiler,
                        &assignment_expr.operator,
                        current_value,
                        value,
                        assignment_expr.value_type.clone_wrapper(),
                    );
                    compiler.builder.build_store(field_ptr, new_value).unwrap();

                    return new_value;
                } else {
                    compiler.builder.build_store(field_ptr, value).unwrap();

                    return value;
                }
            } else {
                let alloca = compiler
                    .named_allocas
                    .get(
                        &assignment_expr
                            .assignee
                            .as_any()
                            .downcast_ref::<TypedSymbolExpr>()
                            .unwrap()
                            .value,
                    )
                    .expect("Variable not found");

                if assignment_expr.operator.kind != TokenKind::Assignment {
                    let current_value = compiler.builder.build_load(*alloca, "").unwrap();
                    value = build_binary_operation(
                        compiler,
                        &assignment_expr.operator,
                        current_value,
                        value,
                        assignment_expr.value_type.clone_wrapper(),
                    );
                }

                compiler.builder.build_store(*alloca, value).unwrap();
            }

            value
        }
        ExprType::StructInit => {
            let struct_init_expr = expression
                .as_any()
                .downcast_ref::<TypedStructInitExpr>()
                .unwrap();

            let struct_type = compiler.named_structs.get(&struct_init_expr.name).unwrap();
            let struct_value = compiler
                .builder
                .build_alloca(struct_type.as_basic_type_enum(), "")
                .unwrap();

            for (i, field) in struct_init_expr.fields.iter().enumerate() {
                let value = gen_expression(compiler, &field.1, None);
                let field_ptr = compiler
                    .builder
                    .build_struct_gep(struct_value, i as u32, "")
                    .unwrap();
                compiler.builder.build_store(field_ptr, value).unwrap();
            }

            struct_value.into()
        }
        _ => todo!(),
    }
}

fn build_binary_operation<'a>(
    compiler: &Compiler<'a>,
    operator: &Token,
    left: BasicValueEnum<'a>,
    right: BasicValueEnum<'a>,
    internal_type: TypeWrapper,
) -> BasicValueEnum<'a> {
    let operator = match operator.kind {
        TokenKind::PlusEquals => Token {
            kind: TokenKind::Plus,
            value: String::from("+"),
            ..operator.clone()
        },
        TokenKind::MinusEquals => Token {
            kind: TokenKind::Dash,
            value: String::from("-"),
            ..operator.clone()
        },
        TokenKind::StarEquals => Token {
            kind: TokenKind::Star,
            value: String::from("*"),
            ..operator.clone()
        },
        TokenKind::SlashEquals => Token {
            kind: TokenKind::Slash,
            value: String::from("/"),
            ..operator.clone()
        },
        _ => operator.clone(),
    };

    match left.get_type() {
        BasicTypeEnum::IntType(_) => match operator.value.as_str() {
            "+" => compiler
                .builder
                .build_int_add(left.into_int_value(), right.into_int_value(), "")
                .unwrap()
                .into(),
            "-" => compiler
                .builder
                .build_int_sub(left.into_int_value(), right.into_int_value(), "")
                .unwrap()
                .into(),
            "*" => compiler
                .builder
                .build_int_mul(left.into_int_value(), right.into_int_value(), "")
                .unwrap()
                .into(),
            "/" => compiler
                .builder
                .build_int_signed_div(left.into_int_value(), right.into_int_value(), "")
                .unwrap()
                .into(),
            "%" => compiler
                .builder
                .build_int_signed_rem(left.into_int_value(), right.into_int_value(), "")
                .unwrap()
                .into(),
            "==" => compiler
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )
                .unwrap()
                .into(),
            "!=" => compiler
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )
                .unwrap()
                .into(),
            "<" => compiler
                .builder
                .build_int_compare(
                    IntPredicate::SLT,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )
                .unwrap()
                .into(),
            "<=" => compiler
                .builder
                .build_int_compare(
                    IntPredicate::SLE,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )
                .unwrap()
                .into(),
            ">" => compiler
                .builder
                .build_int_compare(
                    IntPredicate::SGT,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )
                .unwrap()
                .into(),
            ">=" => compiler
                .builder
                .build_int_compare(
                    IntPredicate::SGE,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )
                .unwrap()
                .into(),
            _ => panic!("Invalid operator {:?}", (operator, left, right, internal_type)),
        },
        // The only exposed pointer type is string for now
        // TODO: Handle other pointer types if needed
        BasicTypeEnum::PointerType(_) => {
            if internal_type.get_type_type() == TypeType::Literal(Literals::String) {
                if operator.value == "+" {
                    // Get size of the two strings added together using the strlen function
                    let size = compiler
                        .builder
                        .build_int_add(
                            // Create an add
                            compiler
                                .builder
                                .build_call(
                                    // Get the length of the left string
                                    compiler.module.get_function("strlen").unwrap(),
                                    &[left.into()],
                                    "",
                                )
                                .unwrap()
                                .try_as_basic_value()
                                .unwrap_left()
                                .into_int_value(),
                            compiler
                                .builder
                                .build_call(
                                    // Get the length of the right string
                                    compiler.module.get_function("strlen").unwrap(),
                                    &[right.into()],
                                    "",
                                )
                                .unwrap()
                                .try_as_basic_value()
                                .unwrap_left()
                                .into_int_value(),
                            "",
                        )
                        .unwrap();

                    let one = compiler.context.i64_type().const_int(1, false);
                    // Add 1 for the null terminator
                    let total_size = compiler.builder.build_int_add(size, one, "").unwrap();

                    // Allocate mutable memory for the new string
                    let new_string = compiler
                        .builder
                        .build_array_alloca(compiler.context.i8_type(), total_size, "")
                        .unwrap();

                    let zero = compiler.context.i8_type().const_int(0, false);
                    let first_byte_ptr = unsafe {
                        compiler
                            .builder
                            .build_gep(new_string, &[compiler.context.i64_type().const_zero()], "")
                            .unwrap()
                    };
                    compiler.builder.build_store(first_byte_ptr, zero).unwrap();

                    // Copy the left string to the new string
                    compiler
                        .builder
                        .build_call(
                            compiler.module.get_function("strcat").unwrap(),
                            &[new_string.into(), left.into()],
                            "",
                        )
                        .unwrap();

                    // Copy the right string to the new string
                    compiler
                        .builder
                        .build_call(
                            compiler.module.get_function("strcat").unwrap(),
                            &[new_string.into(), right.into()],
                            "",
                        )
                        .unwrap();

                    // Return the new string
                    return new_string.into();
                } else if operator.value == "==" {
                    let result = compiler
                        .builder
                        .build_call(
                            compiler.module.get_function("strcmp").unwrap(),
                            &[left.into(), right.into()],
                            "",
                        )
                        .unwrap();
                    return compiler
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            result.try_as_basic_value().left().unwrap().into_int_value(),
                            compiler.context.i32_type().const_zero(), // strcmp returns 0 when equal
                            "",
                        )
                        .unwrap()
                        .into();
                } else if operator.value == "!=" {
                    let result = compiler
                        .builder
                        .build_call(
                            compiler.module.get_function("strcmp").unwrap(),
                            &[left.into(), right.into()],
                            "",
                        )
                        .unwrap();
                    return compiler
                        .builder
                        .build_int_compare(
                            IntPredicate::NE, // Not equal
                            result.try_as_basic_value().left().unwrap().into_int_value(),
                            compiler.context.i32_type().const_zero(),
                            "",
                        )
                        .unwrap()
                        .into();
                } else {
                    panic!("Invalid operator for string");
                }
            }
            panic!("Unknown pointer type");
        }
        _ => panic!("Invalid type for binary operation"),
    }
}
