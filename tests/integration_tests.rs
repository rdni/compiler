//! Integration tests for end-to-end compilation.
//!
//! These tests verify that the complete compilation pipeline works correctly
//! from source code through tokenization, parsing, type checking, and LLVM IR generation.

use compiler::{
    ast::ast::Stmt,
    compiler::compiler::compile,
    lexer::lexer::tokenize,
    parser::parser::parse,
    type_checker::{type_checker::type_check, typed_ast::TypedBlockStmt},
};
use inkwell::context::Context;
use std::path::PathBuf;

#[test]
fn test_compile_simple_program() {
    let source = "let x = 42;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    // Compile to LLVM IR
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_simple.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_function() {
    let source = "fn add(a: i32, b: i32) -> i32 { return a + b; }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_function.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_struct() {
    let source = "struct Point { x: i32, y: i32 }".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_struct.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_control_flow() {
    let source = r#"
        let x = 10;
        if x > 5 {
            x = x + 1;
        } else {
            x = x - 1;
        }
    "#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_control_flow.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_while_loop() {
    let source = r#"
        let i = 0;
        while i < 10 {
            i = i + 1;
        }
    "#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_while_loop.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_struct_with_methods() {
    let source = r#"
        struct Counter {
            value: i32
        }
        
        fn increment(c: Counter) -> i32 {
            return c.value + 1;
        }
    "#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_struct_methods.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_nested_expressions() {
    let source = "let result = (5 + 3) * (10 - 2) / 4;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_nested_expr.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_string_concatenation() {
    let source = r#"let greeting = "Hello, " + "World!";"#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_string_concat.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_member_access() {
    let source = r#"
        struct Point { x: i32, y: i32 }
        let p = new Point { x: 10, y: 20 };
        let x_val = p.x;
    "#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_member_access.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_multiple_functions() {
    let source = r#"
        fn add(a: i32, b: i32) -> i32 {
            return a + b;
        }
        
        fn subtract(a: i32, b: i32) -> i32 {
            return a - b;
        }
        
        let result = add(10, subtract(20, 5));
    "#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_multiple_functions.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_lex_error_invalid_token() {
    let source = "let x = @;".to_string();
    let result = tokenize(source, Some("test.lang".to_string()));
    assert!(result.is_err(), "Should fail on invalid token");
}

#[test]
fn test_parse_error_missing_semicolon() {
    let source = "let x = 42".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(result.is_err(), "Should fail on missing semicolon");
}

#[test]
fn test_parse_error_unexpected_token() {
    let source = "let = 42;".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, result) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(result.is_err(), "Should fail on unexpected token");
}

#[test]
fn test_compile_assignment_operators() {
    let source = r#"
        let x = 10;
        x += 5;
        x -= 3;
    "#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_assignment_ops.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_empty_source() {
    let source = "".to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_empty.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}

#[test]
fn test_compile_comments() {
    let source = r#"
        // This is a comment
        let x = 42; // inline comment
        // Another comment
    "#.to_string();
    let tokens = tokenize(source, Some("test.lang".to_string())).unwrap();
    let (_, ast) = parse(tokens, std::rc::Rc::new("test.lang".to_string()));
    assert!(ast.is_ok());
    
    let ast = ast.unwrap();
    let (type_checker, error) = type_check(ast, None, false);
    assert!(error.is_none(), "Type checking should succeed");
    
    let context = Context::create();
    let typed_ast = type_checker.typed_ast[0]
        .as_any()
        .downcast_ref::<TypedBlockStmt>()
        .unwrap()
        .clone();
    
    let result = compile(
        typed_ast,
        type_checker,
        vec![],
        PathBuf::from("/tmp/compiler_tests/test_comments.ll"),
        "test.lang",
        &context,
    );
    assert!(result.is_ok(), "Compilation should succeed");
}
