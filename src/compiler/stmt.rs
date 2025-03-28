use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};

use crate::{ast::{ast::{Stmt, StmtType, Type, TypeType}, types::Literals}, type_checker::typed_ast::{TypedBlockStmt, TypedExpr, TypedExpressionStmt, TypedFnDeclStmt, TypedIfStmt, TypedReturnStmt, TypedStmtWrapper, TypedStructDeclStmt, TypedVarDeclStmt}};

use super::{compiler::Compiler, expr::gen_expression};

pub fn gen_statement<'a>(compiler: &mut Compiler<'a>, statement: &TypedStmtWrapper) {
    match statement.get_stmt_type() {
        StmtType::ExpressionStmt => { gen_expression(compiler, &statement.as_any().downcast_ref::<TypedExpressionStmt>().unwrap().expression); },
        StmtType::VarDeclStmt => {
            let var_decl_stmt = statement.as_any().downcast_ref::<TypedVarDeclStmt>().unwrap();
            let var_name = var_decl_stmt.identifier.clone();
            let var_value = gen_expression(compiler, var_decl_stmt.assigned_value.as_ref().unwrap());

            let var = compiler.builder.build_alloca(compiler.convert_type(var_decl_stmt.assigned_value.as_ref().unwrap().get_type().get_type_type()), &var_name).unwrap();
            compiler.named_allocas.insert(var_name, var);
            compiler.builder.build_store(var, var_value).unwrap();
        },
        StmtType::IfStmt => {
            let if_stmt = statement.as_any().downcast_ref::<TypedIfStmt>().unwrap();
            let condition = gen_expression(compiler, &if_stmt.condition);
            assert!(condition.get_type() == compiler.context.bool_type().into());
            
            let parent_function = compiler.builder.get_insert_block().unwrap().get_parent().unwrap();
            let current_position = compiler.builder.get_insert_block().unwrap();
            let then_block = compiler.context.append_basic_block(parent_function, "then");

            let end_block = compiler.context.append_basic_block(parent_function, "end");

            if if_stmt.then_body.get_stmt_type() == StmtType::BlockStmt {
                compiler.builder.position_at_end(then_block);

                let then_body = if_stmt.then_body.as_any().downcast_ref::<TypedBlockStmt>().unwrap();
                for stmt in then_body.body.iter() {
                    gen_statement(compiler, stmt);
                }
            } else {
                compiler.builder.position_at_end(then_block);

                gen_statement(compiler, &if_stmt.then_body);
            }

            let mut else_block= None;
            if if_stmt.else_body.is_some() {
                else_block = Some(compiler.context.append_basic_block(compiler.builder.get_insert_block().unwrap().get_parent().unwrap(), "else"));
                compiler.builder.position_at_end(else_block.unwrap());

                let else_body = if_stmt.else_body.as_ref().unwrap().as_any().downcast_ref::<TypedBlockStmt>().unwrap();
                for stmt in else_body.body.iter() {
                    gen_statement(compiler, stmt);
                }

                compiler.builder.build_unconditional_branch(end_block).unwrap();

                compiler.builder.position_at_end(then_block);
                compiler.builder.build_unconditional_branch(end_block).unwrap();
            }

            compiler.builder.position_at_end(current_position);
            if if_stmt.else_body.is_none() {
                compiler.builder.build_conditional_branch(condition.into_int_value(), then_block, end_block).unwrap();
            } else {
                compiler.builder.build_conditional_branch(condition.into_int_value(), then_block, else_block.unwrap()).unwrap();
            }

            compiler.builder.position_at_end(end_block);
        },
        StmtType::FnDeclStmt => {
            let previous_position = compiler.builder.get_insert_block();

            let fn_decl_stmt = statement.as_any().downcast_ref::<TypedFnDeclStmt>().unwrap();
            let params: Vec<BasicMetadataTypeEnum<'a>> = fn_decl_stmt.parameters.iter().map(|val| compiler.convert_type(val.1.get_type_type()).into()).collect();
            
            let function_type;
            if let TypeType::Literal(Literals::Null) = fn_decl_stmt.return_type.get_type_type() {
                function_type = compiler.context.void_type().fn_type(params.as_slice(), false);
            } else {
                function_type = compiler.convert_type(fn_decl_stmt.return_type.clone_wrapper().get_type_type()).fn_type(params.as_slice(), false);
            }

            let function = compiler.create_function(&fn_decl_stmt.identifier, function_type);

            let params = function.get_params();
            let mut args = params.iter();
            for arg in fn_decl_stmt.parameters.iter() {
                let alloca = compiler.builder.build_alloca(compiler.convert_type(arg.1.get_type_type()), &arg.0).unwrap();
                compiler.builder.build_store(alloca, *args.next().unwrap()).unwrap();
                compiler.named_allocas.insert(arg.0.clone(), alloca);
            }

            let body = fn_decl_stmt.body.as_any().downcast_ref::<TypedBlockStmt>().unwrap();
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
        },
        StmtType::ReturnStmt => {
            let return_stmt = statement.as_any().downcast_ref::<TypedReturnStmt>().unwrap();
            let value = gen_expression(compiler, return_stmt.value.as_ref().unwrap());

            compiler.builder.build_return(Some(&value)).unwrap();
        },
        StmtType::StructDeclStmt => {
            let struct_decl_stmt = statement.as_any().downcast_ref::<TypedStructDeclStmt>().unwrap();
            let fields = struct_decl_stmt.fields.iter().map(|(_id, ty)| {
                let ty = compiler.convert_type(ty.get_type_type());
                ty
            }).collect::<Vec<BasicTypeEnum<'a>>>();

            let struct_type = compiler.context.opaque_struct_type(&struct_decl_stmt.name);
            struct_type.set_body(fields.as_slice(), false);

            compiler.named_structs.insert(struct_decl_stmt.name.clone(), struct_type);
        }
        _ => {
            todo!()
        }
    }
}