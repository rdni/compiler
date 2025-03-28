use std::{env, fs::{self, create_dir, read_to_string}, path::PathBuf, process::Command, rc::Rc, time::Instant};

use compiler::{ast::ast::Stmt, compiler::{compiler::compile, stdlib::compile_stdlib}, errors::errors::{Error, ErrorTip}, get_line_at_position, lexer::lexer::tokenize, parser::parser::parse, type_checker::{type_checker::type_check, typed_ast::TypedBlockStmt}};
use inkwell::context::Context;

fn main() {
    if !PathBuf::from("build").exists() {
        create_dir("build").unwrap();
    } else {
        for entry in fs::read_dir("build").unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            fs::remove_file(path).unwrap();
        }
    }

    let context = Context::create();
    let stdlib = compile_stdlib(PathBuf::from("std/std.lang"), PathBuf::from("build/stdlib.ll"), &context);

    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Incorrect arguments provided!");
    }

    let file_path: &str = &args[1];
    let file_name= if file_path.contains("/") {
        file_path.split("/").last().unwrap()
    } else {
        file_path
    };

    let start = Instant::now();

    let mut path_buf_string = env::current_dir().unwrap().into_os_string();
    path_buf_string.push(file_path);
    let file_contents = read_to_string(path_buf_string.clone()).expect("Failed to read file!");

    let tokens = tokenize(file_contents, Some(String::from(file_name)));

    if tokens.is_err() {
        display_error(tokens.err().unwrap(), PathBuf::from(path_buf_string));
        panic!()
    }

    println!("Tokenized in {:?}", start.elapsed());

    let parse_start = Instant::now();
    let parsed_ast = parse(tokens.unwrap(), Rc::new(String::from(file_name)));

    println!("Parsed in {:?}", parse_start.elapsed());

    if parsed_ast.1.is_err() {
        display_error(parsed_ast.1.err().unwrap(), PathBuf::from(path_buf_string));
        panic!()
    }

    let ast = parsed_ast.1.unwrap();

    let type_check_start = Instant::now();
    let type_checker = type_check(ast.clone(), false);

    println!("Type checked in {:?}", type_check_start.elapsed());

    if type_checker.1.is_some() {
        display_error(type_checker.1.unwrap(), PathBuf::from(path_buf_string));
        panic!()
    }

    let type_checker = type_checker.0;


    let compile_start = Instant::now();
    let compiled = compile(type_checker.typed_ast[0].as_any().downcast_ref::<TypedBlockStmt>().unwrap().clone(), type_checker, PathBuf::from("build/out.ll"), file_name, &context).unwrap();

    println!("Compiled in {:?}", compile_start.elapsed());
    println!("Total time for IR generation: {:?}", start.elapsed());

    compiled.module.link_in_module(stdlib.module).unwrap();

    compiled.save_module_to_file(PathBuf::from("build/out.ll"));

    println!("Linked stdlib and main file");

    let llc_result = Command::new("llc")
        .args(["-filetype=obj",  "-relocation-model=pic", "build/out.ll", "-o",  "build/out.o"])
        .output()
        .expect("Failed to compile using llc");

    if !llc_result.stderr.is_empty() {
        panic!("Failed to compile using llc:\n{:?}", String::from_utf8(llc_result.stderr));
    }

    println!("Compiled using LLC");

    let clang_result = Command::new("clang")
        .args(["-fPIE", "-pie", "-o", "build/output", "build/out.o"])
        .output()
        .expect("Failed to compile using clang");
    println!("Compiled using Clang");

    if !clang_result.stderr.is_empty() {
        panic!("Failed to compile using clang:\n{:?}", String::from_utf8(clang_result.stderr));
    }

    println!("Total time: {:?}", start.elapsed());
}

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
            },
            '(' | '[' => {
                indent += 1;
                result.push(c);
                result.push('\n');
                result.push_str(&"  ".repeat(indent));
            },
            '}' | ')' | ']' => {
                indent -= 1;
                result.push('\n');
                result.push_str(&"  ".repeat(indent));
                result.push(c);
            },
            ',' => {
                result.push(c);
                result.push('\n');
                result.push_str(&"  ".repeat(indent));
                ignore_next_space = true;
            },
            ' ' if ignore_next_space => {
                ignore_next_space = false;
            },
            _ => result.push(c),
        }
    }

    result
}

fn display_error(error: Error, file: PathBuf) {
    /*
        error: message
        -> final.lang
           |
        20 | let a = #;
           | --------^
    */

    let position = error.get_position();
    let (line, line_text, line_pos) = get_line_at_position(file.clone(), position.0);

    let line_str = line.to_string();
    let padding = line_str.len() + 2;

    if let ErrorTip::None = error.get_tip() {
        println!("Error: {}", error.get_error_name());
    } else {
        println!("Error: {} ({})", error.get_error_name(), error.get_tip());
    }
    println!("-> {}", file.as_os_str().to_string_lossy());
    println!("{:>padding$}", "|");

    let (line_text_removed, removed_whitespace) = remove_starting_whitespace(&line_text);
    println!("{} | {}", line_str, line_text_removed.trim());

    let arrows = line_pos - removed_whitespace + 1;

    println!("{:>padding$} {:->arrows$}", "|", "^");
}

fn remove_starting_whitespace(string: &str) -> (String, usize) {
    let mut start = 0;
    for c in string.chars() {
        if c == ' ' {
            start += 1;
        } else {
            break;
        }
    }

    (String::from(&string[start..]), start)
}