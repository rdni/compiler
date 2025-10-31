use inkwell::{
    module::Linkage,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    AddressSpace,
};

use crate::{
    ast::{
        ast::{Stmt, StmtType, Type, TypeType},
        types::Literals,
    },
    type_checker::typed_ast::{
        TypedBlockStmt, TypedDropStmt, TypedExpr, TypedExpressionStmt, TypedExternDeclStmt,
        TypedFnDeclStmt, TypedIfStmt, TypedReturnStmt, TypedStmtWrapper, TypedStructDeclStmt,
        TypedVarDeclStmt, TypedWhileStmt,
    },
};

use super::{compiler::Compiler, expr::gen_expression};

pub fn gen_statement<'a>(compiler: &mut Compiler<'a>, statement: &TypedStmtWrapper) {
    match statement.get_stmt_type() {
        StmtType::ExpressionStmt => {
            gen_expression(
                compiler,
                &statement
                    .as_any()
                    .downcast_ref::<TypedExpressionStmt>()
                    .unwrap()
                    .expression,
                None,
            );
        }
        StmtType::VarDeclStmt => {
            let var_decl_stmt = statement
                .as_any()
                .downcast_ref::<TypedVarDeclStmt>()
                .unwrap();

            let var_name = var_decl_stmt.identifier.clone();

            let var_value;
            if var_decl_stmt
                .assigned_value
                .as_ref()
                .unwrap()
                .get_type()
                .get_type_type()
                != var_decl_stmt.var_type.get_type_type()
                && compiler.is_stdlib
            {
                let expected_type = var_decl_stmt.var_type.get_type_type();

                var_value = gen_expression(
                    compiler,
                    var_decl_stmt.assigned_value.as_ref().unwrap(),
                    Some(expected_type),
                );
            } else {
                var_value = gen_expression(
                    compiler,
                    var_decl_stmt.assigned_value.as_ref().unwrap(),
                    None,
                );
            }

            let var = compiler
                .builder
                .build_alloca(
                    compiler.convert_type(var_decl_stmt.var_type.get_type_type()),
                    &var_name,
                )
                .unwrap();
            compiler.named_allocas.insert(var_name, var);

            compiler.builder.build_store(var, var_value).unwrap();
        }
        StmtType::IfStmt => {
            let if_stmt = statement.as_any().downcast_ref::<TypedIfStmt>().unwrap();
            let condition = gen_expression(compiler, &if_stmt.condition, None);
            assert!(condition.get_type() == compiler.context.bool_type().into());

            let parent_function = compiler
                .builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap();
            let current_position = compiler.builder.get_insert_block().unwrap();
            let then_block = compiler.context.append_basic_block(parent_function, "then");

            let end_block = compiler.context.append_basic_block(parent_function, "end");

            if if_stmt.then_body.get_stmt_type() == StmtType::BlockStmt {
                compiler.builder.position_at_end(then_block);

                let then_body = if_stmt
                    .then_body
                    .as_any()
                    .downcast_ref::<TypedBlockStmt>()
                    .unwrap();
                for stmt in then_body.body.iter() {
                    gen_statement(compiler, stmt);
                }

                compiler
                    .builder
                    .build_unconditional_branch(end_block)
                    .unwrap();
            } else {
                compiler.builder.position_at_end(then_block);

                gen_statement(compiler, &if_stmt.then_body);

                compiler
                    .builder
                    .build_unconditional_branch(end_block)
                    .unwrap();
            }

            let mut else_block = None;
            if if_stmt.else_body.is_some() {
                else_block = Some(
                    compiler.context.append_basic_block(
                        compiler
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap(),
                        "else",
                    ),
                );
                compiler.builder.position_at_end(else_block.unwrap());

                let else_body = if_stmt
                    .else_body
                    .as_ref()
                    .unwrap()
                    .as_any()
                    .downcast_ref::<TypedBlockStmt>()
                    .unwrap();
                for stmt in else_body.body.iter() {
                    gen_statement(compiler, stmt);
                }

                compiler
                    .builder
                    .build_unconditional_branch(end_block)
                    .unwrap();
            }

            compiler.builder.position_at_end(current_position);
            if if_stmt.else_body.is_none() {
                compiler
                    .builder
                    .build_conditional_branch(condition.into_int_value(), then_block, end_block)
                    .unwrap();
            } else {
                compiler
                    .builder
                    .build_conditional_branch(
                        condition.into_int_value(),
                        then_block,
                        else_block.unwrap(),
                    )
                    .unwrap();
            }

            compiler.builder.position_at_end(end_block);
        }
        StmtType::FnDeclStmt => {
            let previous_position = compiler.builder.get_insert_block();

            let fn_decl_stmt = statement
                .as_any()
                .downcast_ref::<TypedFnDeclStmt>()
                .unwrap();
            let params: Vec<BasicMetadataTypeEnum<'a>> = fn_decl_stmt
                .parameters
                .iter()
                .map(|val| compiler.convert_type(val.1.get_type_type()).into())
                .collect();

            let function_type;
            if let TypeType::Literal(Literals::Null) = fn_decl_stmt.return_type.get_type_type() {
                function_type = compiler
                    .context
                    .void_type()
                    .fn_type(params.as_slice(), false);
            } else {
                function_type = compiler
                    .convert_type(fn_decl_stmt.return_type.clone_wrapper().get_type_type())
                    .fn_type(params.as_slice(), false);
            }

            let function = compiler.create_function(&fn_decl_stmt.identifier, function_type);

            let params = function.get_params();
            let mut args = params.iter();
            for arg in fn_decl_stmt.parameters.iter() {
                let alloca = compiler
                    .builder
                    .build_alloca(compiler.convert_type(arg.1.get_type_type()), &arg.0)
                    .unwrap();
                compiler
                    .builder
                    .build_store(alloca, *args.next().unwrap())
                    .unwrap();
                compiler.named_allocas.insert(arg.0.clone(), alloca);
            }

            let body = fn_decl_stmt
                .body
                .as_any()
                .downcast_ref::<TypedBlockStmt>()
                .unwrap();
            for stmt in body.body.iter() {
                gen_statement(compiler, stmt);
            }

            if fn_decl_stmt.return_type.get_type_type() == TypeType::Literal(Literals::Null) {
                compiler.builder.build_return(None).unwrap();
            }

            if let Some(pos) = previous_position {
                compiler.builder.position_at_end(pos);
            } else {
                compiler.builder.clear_insertion_position();
            }
        }
        StmtType::ExternDeclStmt => {
            let extern_decl_stmt = statement
                .as_any()
                .downcast_ref::<TypedExternDeclStmt>()
                .unwrap();
            let params: Vec<BasicMetadataTypeEnum<'a>> = extern_decl_stmt
                .parameters
                .iter()
                .map(|val| compiler.convert_type(val.1.get_type_type()).into())
                .collect();

            let function_type;
            if let TypeType::Literal(Literals::Null) = extern_decl_stmt.return_type.get_type_type()
            {
                function_type = compiler
                    .context
                    .void_type()
                    .fn_type(params.as_slice(), extern_decl_stmt.is_variadic);
            } else {
                function_type = compiler
                    .convert_type(extern_decl_stmt.return_type.clone_wrapper().get_type_type())
                    .fn_type(params.as_slice(), extern_decl_stmt.is_variadic);
            }

            compiler.module.add_function(
                &extern_decl_stmt.identifier,
                function_type,
                Some(Linkage::External),
            );
        }
        StmtType::ReturnStmt => {
            let return_stmt = statement
                .as_any()
                .downcast_ref::<TypedReturnStmt>()
                .unwrap();
            let value = gen_expression(compiler, return_stmt.value.as_ref().unwrap(), None);

            compiler.builder.build_return(Some(&value)).unwrap();
        }
        StmtType::StructDeclStmt => {
            let struct_decl_stmt = statement
                .as_any()
                .downcast_ref::<TypedStructDeclStmt>()
                .unwrap();
            let fields = struct_decl_stmt
                .fields
                .iter()
                .map(|(_id, ty)| {
                    let ty = compiler.convert_type(ty.get_type_type());
                    ty
                })
                .collect::<Vec<BasicTypeEnum<'a>>>();

            let struct_type = compiler.context.opaque_struct_type(&struct_decl_stmt.name);
            struct_type.set_body(fields.as_slice(), false);

            let position_before = compiler.builder.get_insert_block();

            // Create the destructor function for the struct
            let destructor_fn_type = compiler.context.void_type().fn_type(
                &[struct_type.ptr_type(AddressSpace::default()).into()],
                false,
            );

            let destructor_fn_name = format!("__{}_destructor__", struct_decl_stmt.name);
            let destructor_fn =
                compiler
                    .module
                    .add_function(&destructor_fn_name, destructor_fn_type, None);

            let entry_block = compiler.context.append_basic_block(destructor_fn, "entry");
            compiler.builder.position_at_end(entry_block);

            for (field_name, field_type) in struct_decl_stmt.fields.iter() {
                match field_type.get_type_type() {
                    TypeType::Struct(ref struct_name) => {
                        // Call the destructor for the field's struct type
                        let field_destructor_fn_name = format!("__{}_destructor__", struct_name);
                        if let Some(field_destructor_fn) =
                            compiler.module.get_function(&field_destructor_fn_name)
                        {
                            let this_ptr = destructor_fn.get_params()[0].into_pointer_value();
                            let field_index = struct_decl_stmt
                                .fields
                                .iter()
                                .position(|(name, _)| name == field_name)
                                .unwrap() as u32;
                            let field_ptr = compiler
                                .builder
                                .build_struct_gep(this_ptr, field_index, "")
                                .unwrap();

                            let field_value = compiler.builder.build_load(field_ptr, "").unwrap();

                            compiler
                                .builder
                                .build_call(
                                    field_destructor_fn,
                                    &[field_value.into_pointer_value().into()],
                                    "",
                                )
                                .unwrap();
                        }
                    }
                    TypeType::Literal(Literals::String) => {
                        // Call the string destructor
                        if let Some(string_destructor_fn) =
                            compiler.module.get_function("__string_destructor__")
                        {
                            let this_ptr = destructor_fn.get_params()[0].into_pointer_value();
                            let field_index = struct_decl_stmt
                                .fields
                                .iter()
                                .position(|(name, _)| name == field_name)
                                .unwrap() as u32;
                            let field_ptr = compiler
                                .builder
                                .build_struct_gep(this_ptr, field_index, "")
                                .unwrap();

                            let field_value = compiler.builder.build_load(field_ptr, "").unwrap();

                            compiler
                                .builder
                                .build_call(
                                    string_destructor_fn,
                                    &[field_value.into_pointer_value().into()],
                                    "",
                                )
                                .unwrap();
                        }
                    }
                    TypeType::Literal(Literals::InternalI8Pointer) => {
                        let free_fn = compiler.module.get_function("free").unwrap();

                        compiler
                            .builder
                            .build_call(
                                free_fn,
                                &[destructor_fn.get_params()[0].into_pointer_value().into()],
                                "",
                            )
                            .unwrap();
                    }
                    _ => {}
                }
            }

            compiler.builder.build_return(None).unwrap();

            compiler.builder.position_at_end(position_before.unwrap());

            compiler
                .named_structs
                .insert(struct_decl_stmt.name.clone(), struct_type);
        }
        StmtType::WhileStmt => {
            let while_stmt = statement.as_any().downcast_ref::<TypedWhileStmt>().unwrap();

            let parent_function = compiler
                .builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap();
            let condition_block = compiler.context.append_basic_block(parent_function, "cond");
            let body_block = compiler.context.append_basic_block(parent_function, "body");
            let end_block = compiler.context.append_basic_block(parent_function, "end");

            compiler
                .builder
                .build_unconditional_branch(condition_block)
                .unwrap();

            compiler.builder.position_at_end(condition_block);
            let condition = gen_expression(compiler, &while_stmt.condition, None);
            assert!(condition.get_type() == compiler.context.bool_type().into());
            compiler
                .builder
                .build_conditional_branch(condition.into_int_value(), body_block, end_block)
                .unwrap();

            compiler.builder.position_at_end(body_block);

            for stmt in while_stmt
                .body
                .as_any()
                .downcast_ref::<TypedBlockStmt>()
                .unwrap()
                .body
                .iter()
            {
                gen_statement(compiler, stmt);
            }

            compiler
                .builder
                .build_unconditional_branch(condition_block)
                .unwrap();

            compiler.builder.position_at_end(end_block);
        }
        StmtType::DropStmt => {
            let drop_stmt = statement.as_any().downcast_ref::<TypedDropStmt>().unwrap();
            let value = gen_expression(compiler, &drop_stmt.expression, None);

            match drop_stmt.expression.get_type().get_type_type() {
                TypeType::Array(_) => {
                    todo!()
                }
                TypeType::Struct(ref struct_name) => {
                    // Call the destructor for the struct type
                    let destructor_fn_name = format!("__{}_destructor__", struct_name);
                    if let Some(destructor_fn) = compiler.module.get_function(&destructor_fn_name) {
                        compiler
                            .builder
                            .build_call(destructor_fn, &[value.into()], "")
                            .unwrap();
                    }
                }
                TypeType::Literal(Literals::String) => {
                    let string_destructor_fn = compiler.module.get_function("__string_destructor__").unwrap();
                    
                    compiler
                        .builder
                        .build_call(
                            string_destructor_fn,
                            &[value.into_pointer_value().into()],
                            "",
                        )
                        .unwrap();
                }
                TypeType::Literal(Literals::InternalI8Pointer) => {
                    let free_fn = compiler.module.get_function("free").unwrap();
                    compiler
                        .builder
                        .build_call(
                            free_fn,
                            &[value.into_pointer_value().into()],
                            "",
                        )
                        .unwrap();
                }
                _ => {
                    // For other types, no action is needed
                }
            }
        }
        _ => {
            println!("Unhandled statement type: {:?}", statement.get_stmt_type());
            todo!()
        }
    }
}
