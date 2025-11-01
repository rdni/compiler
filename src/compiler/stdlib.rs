//! Standard library compilation module.
//!
//! This module handles the compilation of the standard library code,
//! which is written in the source language and compiled to LLVM IR.
//! The stdlib provides core functionality like memory management functions.

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

/// Compiles the standard library from source code.
///
/// This function takes the main stdlib source file, tokenizes, parses,
/// type-checks, and compiles it into LLVM IR. The stdlib is compiled
/// separately from user code to provide core functionality.
///
/// # Arguments
///
/// * `main_stdlib_file` - Path to the main stdlib source file
/// * `output_file` - Path where the compiled stdlib IR should be saved
/// * `context` - LLVM context for code generation
///
/// # Returns
///
/// Returns a Compiler instance containing the compiled stdlib module.
///
/// # Panics
///
/// Panics if:
/// - The stdlib file cannot be read
/// - Parsing fails
/// - Type checking fails
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

/// Compiles the standard library AST into LLVM IR.
///
/// Iterates through all statements in the stdlib AST and generates
/// LLVM IR code for each one.
///
/// # Arguments
///
/// * `compiler` - Mutable reference to the compiler instance
/// * `output_file` - Path where the compiled module will be saved
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

/// Declares external C functions needed by the stdlib.
///
/// This includes functions like `abort`, `panic`, `strcat`, and `strcmp`
/// that are either provided by the C standard library or implemented
/// elsewhere in the runtime.
///
/// # Arguments
///
/// * `compiler` - Mutable reference to the compiler instance
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
