//! Token definitions and types for the lexer.
//!
//! This module defines the token types used throughout the compilation process.
//! It includes:
//!
//! - `TokenKind` enum defining all possible token types
//! - `Token` struct holding token kind, value, and source position
//! - `RESERVED_LOOKUP` mapping reserved keywords to their token kinds
//!
//! Tokens represent the atomic units of the source code after lexical analysis.

use lazy_static::lazy_static;
use std::{collections::HashMap, fmt::Display};

use crate::Span;

lazy_static! {
    /// Lookup table for reserved keywords.
    ///
    /// Maps keyword strings to their corresponding TokenKind variants.
    /// Used by the lexer to distinguish keywords from regular identifiers.
    pub static ref RESERVED_LOOKUP: HashMap<&'static str, TokenKind> = {
        let mut map = HashMap::new();
        map.insert("let", TokenKind::Let);
        map.insert("const", TokenKind::Const);
        map.insert("new", TokenKind::New);
        map.insert("import", TokenKind::Import);
        map.insert("from", TokenKind::From);
        map.insert("return", TokenKind::Return);
        map.insert("fn", TokenKind::Fn);
        map.insert("if", TokenKind::If);
        map.insert("else", TokenKind::Else);
        map.insert("foreach", TokenKind::Foreach);
        map.insert("while", TokenKind::While);
        map.insert("for", TokenKind::For);
        map.insert("export", TokenKind::Export);
        map.insert("typeof", TokenKind::Typeof);
        map.insert("in", TokenKind::In);
        map.insert("struct", TokenKind::Struct);
        map.insert("extern", TokenKind::Extern);
        map
    };
}

/// Represents the different kinds of tokens in the language.
///
/// This enum defines all possible token types including literals,
/// operators, keywords, and punctuation.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenKind {
    EOF,
    Number,
    String,
    Identifier,

    Tilde,

    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,

    Assignment, // =
    Equals,     // ==
    Not,        // !
    NotEquals,  // !=

    LessThan,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,

    Or,
    And,

    Dot,
    // DotDot,
    Ellipsis,
    Semicolon,
    Colon,
    Question,
    Comma,
    Arrow,

    PlusPlus,
    MinusMinus,
    PlusEquals,
    MinusEquals,
    SlashEquals,
    StarEquals,

    Plus,
    Dash,
    Slash,
    Star,
    Percent,

    // Reserved
    Let,
    Const,
    New,
    Import,
    From,
    Fn,
    Return,
    If,
    Else,
    Foreach,
    While,
    For,
    Break,
    Export,
    Typeof,
    In,
    Struct,
    Extern,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Represents a token with its kind, value, and source location.
///
/// Tokens are produced by the lexer and consumed by the parser.
/// Each token includes position information for error reporting.
#[derive(Debug, Clone)]
pub struct Token {
    /// The type of token
    pub kind: TokenKind,
    /// The string value of the token
    pub value: String,
    /// The source code span where this token appears
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {{\nkind: {},\nvalue: {}}}", self.kind, self.value)
    }
}

impl Token {
    /// Checks if the token is one of the specified kinds.
    ///
    /// # Arguments
    ///
    /// * `tokens` - Vector of TokenKind variants to check against
    ///
    /// # Returns
    ///
    /// Returns true if the token matches any of the provided kinds.
    fn is_one_of_many(&self, tokens: Vec<TokenKind>) -> bool {
        for token in tokens {
            if token == self.kind {
                return true;
            }
        }

        false
    }

    /// Prints debug information about this token.
    ///
    /// Displays the token kind and value (for string, identifier, and number tokens).
    pub fn debug(&self) {
        if self.is_one_of_many(vec![
            TokenKind::String,
            TokenKind::Identifier,
            TokenKind::Number,
        ]) {
            println!("{} ({})", self.kind, self.value);
        } else {
            println!("{} ()", self.kind);
        }
    }
}
