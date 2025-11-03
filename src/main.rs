//! Compiler executable entry point.
//!
//! This is the main driver for the compiler. It orchestrates the compilation
//! pipeline from source code to executable binary:
//!
//! 1. Sets up the build directory
//! 2. Compiles the standard library
//! 3. Lexes and parses the input source file
//! 4. Type checks the AST
//! 5. Generates LLVM IR
//! 6. Links with the standard library
//! 7. Compiles to object code with LLC
//! 8. Links to executable with Clang
//!
//! Usage: compiler <source_file>

use std::{
    env,
    fs::{self, create_dir, read_to_string},
    path::PathBuf,
    process::Command,
    rc::Rc,
    time::Instant,
};

use compiler::{
    ast::{
        ast::{Stmt, Type, TypeType, TypeWrapper},
        types::FunctionType,
    },
    compiler::{
        compiler::{compile, Compiler},
        stdlib::compile_stdlib,
    },
    display_error,
    lexer::lexer::tokenize,
    parser::parser::parse,
    type_checker::{type_checker::type_check, typed_ast::TypedBlockStmt},
};
use inkwell::context::Context;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Incorrect arguments provided!");
    }


    if !PathBuf::from("build").exists() {
        create_dir("build").unwrap();
    } else {
        // clear build dir
        for entry in fs::read_dir("build").unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            fs::remove_file(path).unwrap();
        }
    }

    let file_path: &str = &args[1];
    let file_name = if file_path.contains("/") {
        file_path.split("/").last().unwrap()
    } else {
        file_path
    };

    let file_contents = read_to_string(file_path).expect("Failed to read file!");

    let context = Context::create();
    let stdlib = compile_stdlib(
        PathBuf::from("std/std.lang"),
        PathBuf::from("build/stdlib.ll"),
        &context,
    );

    let stdlib_functions = convert_stdlib_functions(&stdlib);

    let start = Instant::now();

    let tokens = tokenize(file_contents, Some(String::from(file_name)));

    if tokens.is_err() {
        display_error(tokens.err().unwrap(), PathBuf::from(file_path));
        panic!()
    }

    println!("Tokenized in {:?}", start.elapsed());

    let parse_start = Instant::now();
    let parsed_ast = parse(tokens.unwrap(), Rc::new(String::from(file_name)));

    println!("Parsed in {:?}", parse_start.elapsed());

    if parsed_ast.1.is_err() {
        display_error(parsed_ast.1.err().unwrap(), PathBuf::from(file_path));
        panic!()
    }

    let ast = parsed_ast.1.unwrap();

    let type_check_start = Instant::now();
    let type_checker = type_check(ast.clone(), Some(&stdlib_functions), false);

    println!("Type checked in {:?}", type_check_start.elapsed());

    if type_checker.1.is_some() {
        display_error(type_checker.1.unwrap(), PathBuf::from(file_path));
        panic!()
    }

    let type_checker = type_checker.0;

    let compile_start = Instant::now();
    let compiled = compile(
        type_checker.typed_ast[0]
            .as_any()
            .downcast_ref::<TypedBlockStmt>()
            .unwrap()
            .clone(),
        type_checker,
        stdlib_functions,
        PathBuf::from("build/out.ll"),
        file_name,
        &context,
    )
    .unwrap();

    println!("Compiled in {:?}", compile_start.elapsed());
    println!("Total time for IR generation: {:?}", start.elapsed());

    compiled.module.link_in_module(stdlib.module).unwrap();

    compiled.save_module_to_file(PathBuf::from("build/out.ll"));

    println!("Linked stdlib and main file");

    let llc_result = Command::new("llc")
        .args([
            "-filetype=obj",
            "-relocation-model=pic",
            "build/out.ll",
            "-o",
            "build/out.o",
        ])
        .output()
        .expect("Failed to compile using llc");

    if !llc_result.stderr.is_empty() {
        panic!(
            "Failed to compile using llc:\n{:?}",
            String::from_utf8(llc_result.stderr)
        );
    }

    println!("Compiled using LLC");

    let clang_result = Command::new("clang")
        .args(["-fPIE", "-o", "build/output", "build/out.o"])
        .output()
        .expect("Failed to compile using clang");
    println!("Compiled using Clang");

    if !clang_result.stderr.is_empty() {
        println!(
            "Error while compiling using clang:\n{}",
            String::from_utf8(clang_result.stderr).unwrap()
        );
    }

    println!("Total time: {:?}", start.elapsed());
}

/// Pretty-prints debug output with indentation (unused utility function).
///
/// Formats strings with proper indentation for debugging purposes.
///
/// # Arguments
///
/// * `string` - The string to format
///
/// # Returns
///
/// A formatted string with proper indentation
#[allow(dead_code)]
fn pretty_print(string: String) -> String {
    let mut result = String::new();
    let mut indent = 0;
    let mut ignore_next_space = false;

    for c in string.chars() {
        match c {
            '{' => {
                indent += 1;
                result.push(c);
                result.push('\n');
                result.push_str(&"  ".repeat(indent));
                ignore_next_space = true;
            }
            '(' | '[' => {
                indent += 1;
                result.push(c);
                result.push('\n');
                result.push_str(&"  ".repeat(indent));
            }
            '}' | ')' | ']' => {
                indent -= 1;
                result.push('\n');
                result.push_str(&"  ".repeat(indent));
                result.push(c);
            }
            ',' => {
                result.push(c);
                result.push('\n');
                result.push_str(&"  ".repeat(indent));
                ignore_next_space = true;
            }
            ' ' if ignore_next_space => {
                ignore_next_space = false;
            }
            _ => result.push(c),
        }
    }

    result
}

/// Extracts function signatures from the compiled standard library.
///
/// Filters the stdlib for exported functions and converts them to
/// type signatures that can be used during compilation of user code.
///
/// # Arguments
///
/// * `stdlib` - Reference to the compiled stdlib compiler instance
///
/// # Returns
///
/// A vector of tuples containing function names and their type signatures
fn convert_stdlib_functions(stdlib: &Compiler<'_>) -> Vec<(String, TypeWrapper)> {
    let mut current_environment = stdlib.type_checker.environments[0].lock().unwrap();

    let mut functions = vec![];
    for base_signature in &mut current_environment.variable_lookup {
        let name = base_signature.0.clone();

        if let TypeType::Function(_) = base_signature.1 .1.get_type_type() {
            let mut signature = base_signature
                .1
                 .1
                .clone_wrapper()
                .as_any()
                .downcast_ref::<FunctionType>()
                .unwrap()
                .clone();

            if signature.should_export {
                signature.is_native = true;
                signature.body = None;
                functions.push((name, signature.clone_wrapper()));
            }
        }
    }

    functions
}
