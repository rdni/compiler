//! Utility macros for the compiler.
//!
//! This module defines helper macros used throughout the compiler:
//!
//! - `MK_TOKEN!` - Creates a Token instance
//! - `MK_DEFAULT_HANDLER!` - Creates a default lexer handler for simple tokens
//!
//! These macros reduce boilerplate in the lexer implementation.

/// Creates a Token instance.
///
/// # Arguments
///
/// * `$kind` - The TokenKind
/// * `$value` - The token's string value
/// * `$span` - The source span
///
/// # Example
///
/// ```ignore
/// let token = MK_TOKEN!(TokenKind::Number, "42".to_string(), span);
/// ```
#[macro_export]
macro_rules! MK_TOKEN {
    ($kind:expr, $value:expr, $span:expr) => {
        Token {
            kind: $kind,
            value: $value,
            span: $span,
        }
    };
}

/// Creates a default lexer handler for simple single-token patterns.
///
/// Generates a handler function that creates a token with the given kind
/// and advances the lexer position by the token's length.
///
/// # Arguments
///
/// * `$kind` - The TokenKind to create
/// * `$value` - The literal string value (used for length calculation)
///
/// # Example
///
/// ```ignore
/// RegexPattern {
///     regex: Regex::new("\\+").unwrap(),
///     handler: MK_DEFAULT_HANDLER!(TokenKind::Plus, "+"),
/// }
/// ```
#[macro_export]
macro_rules! MK_DEFAULT_HANDLER {
    ($kind:expr, $value:literal) => {
        |lexer: &mut Lexer, _regex: Regex| {
            lexer.push(MK_TOKEN!(
                $kind,
                String::from($value),
                Span {
                    start: Position(lexer.pos as u32, Rc::clone(&lexer.file)),
                    end: Position(
                        (lexer.pos + $value.len() as i32) as u32,
                        Rc::clone(&lexer.file)
                    )
                }
            ));
            lexer.advance_n($value.len().try_into().unwrap());
        }
    };
}
