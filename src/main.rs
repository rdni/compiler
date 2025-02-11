use std::{env, fs::{self, create_dir, read_to_string}, path::PathBuf, process::Command, rc::Rc, time::Instant};

use compiler::{ast::ast::Stmt, compiler::{compiler::compile, stdlib::compile_stdlib}, errors::errors::Error, lexer::lexer::tokenize, parser::parser::parse, type_checker::{type_checker::type_check, typed_ast::TypedBlockStmt}};

fn main() {
    if !PathBuf::from("build").exists() {
        create_dir("build").unwrap();
    }

    compile_stdlib(PathBuf::from("std/std.lang"), PathBuf::from("build/stdlib.ll"));

    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Incorrect arguments provided!");
    }

    let file_path: &str = &args[1];
    let file_name;

    if file_path.contains("/") {
        file_name = file_path.split("/").into_iter().last().unwrap();
    } else {
        file_name = file_path;
    }

    let start = Instant::now();

    let mut path_buf_string = env::current_dir().unwrap().into_os_string().to_owned();
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
    let type_checker = type_check(ast.clone());

    println!("Type checked in {:?}", type_check_start.elapsed());

    if type_checker.1.is_some() {
        display_error(type_checker.1.unwrap(), PathBuf::from(path_buf_string));
        panic!()
    }

    let type_checker = type_checker.0;

    let compile_start = Instant::now();
    compile(type_checker.typed_ast[0].as_any().downcast_ref::<TypedBlockStmt>().unwrap().clone(), type_checker, PathBuf::from("build/out.ll"), file_name).unwrap();

    println!("Compiled in {:?}", compile_start.elapsed());
    println!("Total time for IR generation: {:?}", start.elapsed());

    let llvm_link_result = Command::new("llvm-link")
        .args(["build/stdlib.ll", "build/out.ll", "-o", "build/linked.bc"])
        .output()
        .expect("Failed to link stdlib and main file");

    if llvm_link_result.stderr.len() != 0 {
        panic!("Failed to link stdlib and main file:\n{:?}", String::from_utf8(llvm_link_result.stderr));
    }

    println!("Linked stdlib and main file");

    let llc_result = Command::new("llc")
        .args(["-filetype=obj",  "-relocation-model=pic", "build/linked.bc", "-o",  "build/out.o"])
        .output()
        .expect("Failed to compile using llc");

    if llc_result.stderr.len() != 0 {
        panic!("Failed to compile using llc:\n{:?}", String::from_utf8(llc_result.stderr));
    }

    println!("Compiled using LLC");

    let clang_result = Command::new("clang")
        .args(["-fPIE", "-pie", "-o", "build/output", "build/out.o"])
        .output()
        .expect("Failed to compile using clang");
    println!("Compiled using Clang");

    if clang_result.stderr.len() != 0 {
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

    println!("Error: {}", error.get_error_name());
    println!("-> {}", file.as_os_str().to_string_lossy());
    println!("{:>padding$}", "|");
    println!("{} | {}", line_str, line_text.trim());

    let arrows = line_pos;

    println!("{:>padding$} {:->arrows$}", "|", "^");
}

pub fn get_line_at_position(file: PathBuf, position: u32) -> (usize, String, usize) {
    let content = fs::read_to_string(&file).unwrap();
    let pos = position as usize;

    if pos >= content.len() {
        panic!("Position exceeds file length");
    }

    let mut start = 0;
    let mut line_number = 1; // 1-based line numbering

    for line in content.split_inclusive('\n') {
        let end = start + line.len();
        
        if (start..end).contains(&pos) {
            let line_pos = pos - start;
            return (line_number, line.to_string(), line_pos);
        }
        
        start = end;
        line_number += 1;
    }

    panic!("Failed to find line containing position");
}