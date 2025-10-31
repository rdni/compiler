use std::{fs, path::PathBuf, rc::Rc};

use inkwell::{context::Context, module::Linkage, AddressSpace};

use crate::{
    ast::ast::Stmt,
    display_error,
    lexer::lexer::tokenize,
    parser::parser::parse,
    type_checker::{
        type_checker::{type_check, Environment},
        typed_ast::{TypedBlockStmt, TypedStmt, TypedStmtWrapper},
    },
};

use super::{compiler::Compiler, stmt::gen_statement};

pub fn compile_stdlib(
    main_stdlib_file: PathBuf,
    output_file: PathBuf,
    context: &Context,
) -> Compiler<'_> {
    // Tokenize, parse, typecheck and compile the main stdlib file
    let main_stdlib =
        fs::read_to_string(main_stdlib_file.clone()).expect("Failed to read main stdlib file");
    let main_stdlib_tokens = tokenize(main_stdlib.clone(), None).unwrap();

    let main_stdlib_ast = parse(main_stdlib_tokens, Rc::new(main_stdlib));

    if main_stdlib_ast.1.is_err() {
        panic!("Failed to parse main stdlib file: {:?}", main_stdlib_ast.1);
    }

    let type_checker = type_check(main_stdlib_ast.1.unwrap(), None, true);

    if type_checker.1.is_some() {
        display_error(type_checker.1.unwrap(), main_stdlib_file);
        panic!("Failed to typecheck main stdlib file");
    }

    let type_checker = type_checker.0;

    // Compile (Not using the normal compile function)
    let mut compiler = Compiler::new(
        true,
        type_checker.typed_ast[0]
            .as_any()
            .downcast_ref::<TypedBlockStmt>()
            .unwrap()
            .clone(),
        type_checker,
        context,
        output_file.as_os_str().to_str().unwrap(),
    );

    compiler.current_environment = compiler.ast.id;
    compiler.type_checker.clear_environment_path();
    compiler
        .type_checker
        .add_environment(Environment::new(compiler.ast.id));
    compiler
        .type_checker
        .set_current_environment(compiler.ast.id);

    declare_external_functions(&mut compiler);

    compile_stdlib_ast(&mut compiler, output_file);

    compiler
}

fn compile_stdlib_ast(compiler: &mut Compiler, output_file: PathBuf) {
    let statements = compiler
        .ast
        .body
        .iter()
        .map(|stmt| stmt.clone_typed_wrapper())
        .collect::<Vec<TypedStmtWrapper>>();
    for statement in statements.iter() {
        gen_statement(compiler, statement);
    }

    compiler.save_module_to_file(output_file);
}

fn declare_external_functions(compiler: &mut Compiler) {
    let i8_ptr_type = compiler.context.i8_type().ptr_type(AddressSpace::default());

    let abort_type = compiler.context.void_type().fn_type(&[], false);
    compiler
        .module
        .add_function("abort", abort_type, Some(Linkage::External));

    compiler.module.add_function(
        "panic",
        compiler.context.void_type().fn_type(
            &[
                compiler
                    .context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .into(),
                compiler.context.i64_type().into(),
            ],
            false,
        ),
        Some(Linkage::External),
    );

    // i8* strcat(i8* dest, i8* src)
    let strcat_type = compiler
        .context
        .i8_type()
        .ptr_type(AddressSpace::default())
        .fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
    compiler
        .module
        .add_function("strcat", strcat_type, Some(Linkage::External));

    let strcmp_type = compiler
        .context
        .i32_type()
        .fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
    compiler
        .module
        .add_function("strcmp", strcmp_type, Some(Linkage::External));
}
