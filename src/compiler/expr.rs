use inkwell::{types::{BasicType, BasicTypeEnum}, values::{BasicMetadataValueEnum, BasicValueEnum}, IntPredicate};

use crate::{ast::{ast::{Expr, ExprType, Type, TypeType}, expressions::{NumberExpr, StringExpr}, types::{self, Literals, StructType}}, type_checker::typed_ast::{TypedAssignmentExpr, TypedBinaryExpr, TypedCallExpr, TypedExpr, TypedExprWrapper, TypedStructInitExpr, TypedSymbolExpr}};

use super::compiler::Compiler;

pub fn gen_expression<'a>(compiler: &Compiler<'a>, expression: &TypedExprWrapper) -> BasicValueEnum<'a> {
    match expression.get_expr_type() {
        ExprType::String => compiler.builder.build_global_string_ptr(&expression.as_any().downcast_ref::<StringExpr>().unwrap().value, "").unwrap().as_pointer_value().into(),
        ExprType::Number => compiler.context.i32_type().const_int(expression.as_any().downcast_ref::<NumberExpr>().unwrap().value as u64, false).into(),
        ExprType::CallExpr => {
            let call_expr = expression.as_any().downcast_ref::<TypedCallExpr>().unwrap();

            let symbol = call_expr.callee.as_any().downcast_ref::<types::FunctionType>().unwrap().name.clone();
            let function = compiler.module.get_function(&symbol).unwrap_or_else(|| {
                panic!("Function {} not found", symbol);
            });
            
            let args: Vec<BasicMetadataValueEnum<'a>> = call_expr.arguments.iter().map(|arg| gen_expression(compiler, arg).into()).collect();

            compiler.builder.build_call(function, &args[..], "").unwrap().try_as_basic_value().left().unwrap_or(compiler.context.i32_type().const_zero().into())
        },
        ExprType::Symbol => {
            let symbol = expression.as_any().downcast_ref::<TypedSymbolExpr>().unwrap();

            let alloca = compiler.named_allocas.get(&symbol.value).expect("Variable not found");

            let value = compiler.builder.build_load(*alloca, &symbol.value).unwrap();

            value
        },
        ExprType::Binary => {
            let binary_expr = expression.as_any().downcast_ref::<TypedBinaryExpr>().unwrap();

            if binary_expr.operator.value == "." {
                // Member expr
                let left = gen_expression(compiler, &binary_expr.left);
                let right = binary_expr.right.as_any().downcast_ref::<TypedSymbolExpr>().unwrap();

                let property_name = right.value.clone();

                if let Some(struct_type) = TypedExpr::get_type(&binary_expr.left).as_any().downcast_ref::<StructType>() {
                    let index = struct_type.fields.iter().position(|field| field.0 == property_name).unwrap() as u32;
                    let field_ptr = compiler.builder.build_struct_gep(left.into_pointer_value(), index, "").unwrap();
                    return compiler.builder.build_load(field_ptr, &property_name).unwrap();
                } else {
                    todo!();
                }

            }
            let left = gen_expression(compiler, &binary_expr.left);
            let right = gen_expression(compiler, &binary_expr.right);

            if left.get_type() != right.get_type() {
                panic!("Type mismatch");
            }

            match left.get_type() {
                BasicTypeEnum::IntType(_) => {
                    match binary_expr.operator.value.as_str() {
                        "+" => compiler.builder.build_int_add(left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        "-" => compiler.builder.build_int_sub(left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        "*" => compiler.builder.build_int_mul(left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        "/" => compiler.builder.build_int_signed_div(left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        "%" => compiler.builder.build_int_signed_rem(left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        "==" => compiler.builder.build_int_compare(IntPredicate::EQ, left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        "!=" => compiler.builder.build_int_compare(IntPredicate::NE, left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        "<" => compiler.builder.build_int_compare(IntPredicate::SLT, left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        "<=" => compiler.builder.build_int_compare(IntPredicate::SLE, left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        ">" => compiler.builder.build_int_compare(IntPredicate::SGT, left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        ">=" => compiler.builder.build_int_compare(IntPredicate::SGE, left.into_int_value(), right.into_int_value(), "").unwrap().into(),
                        _ => panic!("Invalid operator")
                    }
                },
                BasicTypeEnum::PointerType(_) => {
                    if TypedExpr::get_type(&binary_expr.left).get_type_type() == TypeType::Literal(Literals::String) {
                        if binary_expr.operator.value == "+" {
                            // Get size of the two strings added together
                            let size = compiler.builder.build_int_add(left.into_pointer_value().get_type().get_element_type().size_of().unwrap(), right.into_pointer_value().get_type().get_element_type().size_of().unwrap(), "").unwrap();

                            // Allocate mutable memory for the new string
                            let new_string = compiler.builder.build_array_alloca(compiler.context.i8_type(), size, "").unwrap();

                            // Copy the left string to the new string
                            compiler.builder.build_call(compiler.module.get_function("strcat").unwrap(), &[new_string.into(), left.into()], "").unwrap();

                            // Copy the right string to the new string
                            compiler.builder.build_call(compiler.module.get_function("strcat").unwrap(), &[new_string.into(), right.into()], "").unwrap();

                            // Return the new string
                            return new_string.into();
                        } else if binary_expr.operator.value == "==" {
                            let result = compiler.builder.build_call(compiler.module.get_function("strcmp").unwrap(), &[gen_expression(compiler, &binary_expr.left).into(), gen_expression(compiler, &binary_expr.right).into()], "").unwrap();
                            return compiler.builder.build_int_compare(IntPredicate::EQ, result.try_as_basic_value().left().unwrap().into_int_value(), compiler.context.i32_type().const_zero(), "").unwrap().into();
                        } else if binary_expr.operator.value == "!=" {
                            let result = compiler.builder.build_call(compiler.module.get_function("strcmp").unwrap(), &[left.into(), right.into()], "").unwrap();
                            return compiler.builder.build_int_compare(IntPredicate::NE, result.try_as_basic_value().left().unwrap().into_int_value(), compiler.context.i32_type().const_zero(), "").unwrap().into();
                        } else {
                            panic!("Invalid operator for string");
                        }
                    }
                    panic!("Unknown pointer type");
                }
                _ => panic!("Invalid type for binary operation")
            }
        },
        ExprType::Assignment => {
            let assignment_expr = expression.as_any().downcast_ref::<TypedAssignmentExpr>().unwrap();

            let value = gen_expression(compiler, &assignment_expr.value);
            if assignment_expr.assignee.as_any().downcast_ref::<TypedBinaryExpr>().is_some() {
                let binary_expr = assignment_expr.assignee.as_any().downcast_ref::<TypedBinaryExpr>().unwrap();
                let struct_value = gen_expression(compiler, &binary_expr.left);
                let field_name = binary_expr.right.as_any().downcast_ref::<TypedSymbolExpr>().unwrap().value.clone();

                let struct_type = TypedExpr::get_type(&binary_expr.left);
                let struct_type = struct_type.as_any().downcast_ref::<StructType>().unwrap();
                let index = struct_type.fields.iter().position(|field| field.0 == field_name).unwrap() as u32;
                let field_ptr = compiler.builder.build_struct_gep(struct_value.into_pointer_value(), index, "").unwrap();
                compiler.builder.build_store(field_ptr, value).unwrap();

                return value;
            } else {
                let alloca = compiler.named_allocas.get(&assignment_expr.assignee.as_any().downcast_ref::<TypedSymbolExpr>().unwrap().value).expect("Variable not found");
                compiler.builder.build_store(*alloca, value).unwrap();
            }

            value
        },
        ExprType::StructInit => {
            let struct_init_expr = expression.as_any().downcast_ref::<TypedStructInitExpr>().unwrap();

            let struct_type = compiler.named_structs.get(&struct_init_expr.name).unwrap();
            let struct_value = compiler.builder.build_alloca(struct_type.as_basic_type_enum(), "").unwrap();

            for (i, field) in struct_init_expr.fields.iter().enumerate() {
                let value = gen_expression(compiler, &field.1);
                let field_ptr = compiler.builder.build_struct_gep(struct_value, i as u32, "").unwrap();
                compiler.builder.build_store(field_ptr, value).unwrap();
            }

            struct_value.into()
        }
        _ => todo!()
    }
}