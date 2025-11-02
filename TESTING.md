# Test Suite Documentation

This document describes the comprehensive test suite for the compiler project.

## Overview

The test suite covers all major components of the compiler with a total of **86 tests** ensuring code correctness and reliability.

## Test Categories

### 1. Lexer Tests (22 tests)
Location: `src/lexer/tests.rs`

Tests for the tokenization phase including:
- **Keywords**: Testing all reserved keywords (let, const, fn, if, else, while, etc.)
- **Identifiers**: Variable names, function names with various patterns
- **Numeric Literals**: Integers and floating-point numbers
- **String Literals**: Including escape sequences (\n, \t, \\, \x hex codes)
- **Operators**: Arithmetic (+, -, *, /, %), comparison (==, !=, <, >, <=, >=), logical (&&, ||)
- **Punctuation**: Parentheses, braces, brackets, semicolons, commas, etc.
- **Compound Operators**: +=, -=, ++, --
- **Comments**: Single-line comments (//)
- **Whitespace Handling**: Proper skipping of whitespace
- **Error Cases**: Unrecognized tokens
- **Escaped Quotes**: Proper handling of \" inside strings (bug fix)

### 2. Parser Tests (25 tests)
Location: `src/parser/tests.rs`

Tests for the parsing phase including:
- **Variable Declarations**: let and const statements
- **Function Declarations**: With parameters and return types
- **Control Flow**: if statements, if-else, while loops
- **Struct Definitions**: Struct declarations and instantiation
- **Expressions**: 
  - Binary expressions (arithmetic, comparison, logical)
  - Unary expressions (negation)
  - Parenthesized expressions
  - Member access (struct.field)
  - Function calls
- **Assignments**: Simple and compound assignments
- **Return Statements**
- **Multiple Statements**: Sequential statement parsing
- **Nested Blocks**: Block scoping
- **Literals**: String and numeric literals
- **Empty Programs**
- **Error Cases**: Missing semicolons, unexpected tokens

### 3. Error Handling Tests (14 tests)
Location: `src/errors/tests.rs`

Tests for error reporting including:
- **Error Creation**: Proper error construction
- **Position Tracking**: Source location information
- **Error Types**:
  - UnrecognisedToken
  - UnexpectedToken
  - TypeMatchError
  - VariableNotDeclared
  - VariableAlreadyDeclared
  - FunctionAlreadyDeclared
  - UnknownType
  - UnexpectedArguments
  - MissingArguments
  - VariableAlreadyDropped
  - NotImplementedError
- **Error Tips**: Suggestions and helpful messages
- **Error Display**: String formatting

### 4. Utility Function Tests (9 tests)
Location: `src/lib.rs`

Tests for utility functions including:
- **Position and Span**: Source position tracking
- **get_line_at_position**: Finding specific lines in files
- **remove_starting_whitespace**: Whitespace trimming for error display
- **Edge Cases**: Empty strings, no whitespace, only whitespace

### 5. Integration Tests (16 tests)
Location: `tests/integration_tests.rs`

End-to-end compilation tests including:
- **Simple Programs**: Basic variable declarations
- **Functions**: Function declarations and calls
- **Structs**: Struct definitions and member access
- **Control Flow**: If-else, while loops
- **Complex Expressions**: Nested expressions, operator precedence
- **String Operations**: Concatenation
- **Multiple Functions**: Function composition
- **Assignment Operators**: +=, -=
- **Comments**: Comment handling
- **Error Cases**: Lexing and parsing errors
- **Empty Source**: Empty program handling

## Running Tests

### Run All Tests
```bash
export LLVM_SYS_140_PREFIX=/usr/lib/llvm-14
cargo test
```

### Run Specific Test Suite
```bash
# Lexer tests only
cargo test lexer

# Parser tests only
cargo test parser

# Integration tests only
cargo test --test integration_tests

# Error tests only
cargo test errors::tests
```

### Run Individual Test
```bash
cargo test test_tokenize_keywords
```

### Run Tests with Output
```bash
cargo test -- --nocapture
```

## Test Coverage Summary

| Component | Tests | Coverage |
|-----------|-------|----------|
| Lexer | 22 | Keywords, literals, operators, strings, escapes, errors |
| Parser | 25 | Statements, expressions, types, error handling |
| Errors | 14 | All error types, tips, formatting |
| Utilities | 9 | Position tracking, string utilities |
| Integration | 16 | End-to-end compilation scenarios |
| **Total** | **86** | **Comprehensive coverage** |

## Key Features Tested

### Language Features
- ✅ Variable declarations (let, const)
- ✅ Function declarations
- ✅ Struct definitions
- ✅ Control flow (if/else, while)
- ✅ Expressions (binary, unary, member access)
- ✅ String literals with escape sequences
- ✅ Numeric literals (int, float)
- ✅ Comments
- ✅ Assignment operators
- ✅ Type annotations

### Error Handling
- ✅ Lexical errors (invalid tokens)
- ✅ Syntax errors (missing semicolons, unexpected tokens)
- ✅ Type errors
- ✅ Undefined variables/functions
- ✅ Duplicate declarations
- ✅ Argument mismatches

### Bug Fixes Included
- ✅ Fixed lexer regex to properly handle escaped quotes in strings (\" now works correctly)

## Testing Best Practices

1. **Isolation**: Each test is independent and doesn't rely on others
2. **Clarity**: Test names clearly describe what is being tested
3. **Coverage**: Both positive (success) and negative (error) cases are tested
4. **Documentation**: Each test file has module-level documentation
5. **Maintainability**: Tests are organized by component for easy navigation

## Future Test Additions

Potential areas for additional testing:
- Type checker comprehensive tests (currently minimal)
- Code generation tests (LLVM IR validation)
- More complex control flow scenarios
- Module/import system (when implemented)
- More edge cases and corner cases
- Performance benchmarks
- Fuzzing tests

## Contributing

When adding new features:
1. Add corresponding unit tests
2. Add integration tests for end-to-end scenarios
3. Update this documentation
4. Ensure all tests pass before committing
