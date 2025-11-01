//! Lookup tables and handler registration for the parser.
//!
//! This module defines the type aliases for handler functions and
//! initializes the lookup tables that map tokens to their parsing handlers.
//! It includes:
//!
//! - Type definitions for NUD, LED, and statement handlers
//! - Binding power (precedence) definitions
//! - Registration functions that populate the parser's lookup tables
//!
//! These lookup tables enable the Pratt parser to dispatch to the
//! appropriate parsing function based on the current token.

use std::collections::HashMap;

use crate::{
    ast::ast::{ExprWrapper, StmtWrapper},
    errors::errors::Error,
    lexer::tokens::TokenKind,
};

use super::{expr::*, parser::Parser, stmt::*};

/// Defines the binding power (precedence) levels for operators.
///
/// Lower values bind less tightly, higher values bind more tightly.
/// Used in Pratt parsing to determine operator precedence.
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub enum BindingPower {
    Default,
    Comma,
    Assignment,
    Logical,
    Relational,
    Additive,
    Multiplicative,
    Unary,
    Call,
    Member,
    Primary,
}

/// Type alias for statement handler functions.
///
/// Statement handlers parse a complete statement and return a StmtWrapper.
pub type StmtHandler = fn(&mut Parser) -> Result<StmtWrapper, Error>;

/// Type alias for null denotation (prefix) handler functions.
///
/// NUD handlers parse prefix expressions (literals, identifiers, prefix operators).
pub type NUDHandler = fn(&mut Parser) -> Result<ExprWrapper, Error>;

/// Type alias for left denotation (infix) handler functions.
///
/// LED handlers parse infix expressions (binary operators, function calls, member access).
pub type LEDHandler = fn(&mut Parser, ExprWrapper, BindingPower) -> Result<ExprWrapper, Error>;

/// Initializes the expression and statement parsing lookup tables.
///
/// Registers all NUD, LED, and statement handlers with the parser
/// for the various token types in the language.
///
/// # Arguments
///
/// * `parser` - Mutable reference to the parser to initialize
pub fn create_token_lookups(parser: &mut Parser) {
    parser.led(
        TokenKind::Assignment,
        BindingPower::Assignment,
        parse_assignment_expr,
    );
    parser.led(
        TokenKind::PlusEquals,
        BindingPower::Assignment,
        parse_assignment_expr,
    );
    parser.led(
        TokenKind::MinusEquals,
        BindingPower::Assignment,
        parse_assignment_expr,
    );
    parser.led(
        TokenKind::StarEquals,
        BindingPower::Assignment,
        parse_assignment_expr,
    );
    parser.led(
        TokenKind::SlashEquals,
        BindingPower::Assignment,
        parse_assignment_expr,
    );

    // Logical
    parser.led(TokenKind::And, BindingPower::Logical, parse_binary_expr);
    parser.led(TokenKind::Or, BindingPower::Logical, parse_binary_expr);

    // Relational
    parser.led(TokenKind::Less, BindingPower::Relational, parse_binary_expr);
    parser.led(
        TokenKind::LessEquals,
        BindingPower::Relational,
        parse_binary_expr,
    );
    parser.led(
        TokenKind::Greater,
        BindingPower::Relational,
        parse_binary_expr,
    );
    parser.led(
        TokenKind::GreaterEquals,
        BindingPower::Relational,
        parse_binary_expr,
    );
    parser.led(
        TokenKind::Equals,
        BindingPower::Relational,
        parse_binary_expr,
    );
    parser.led(
        TokenKind::NotEquals,
        BindingPower::Relational,
        parse_binary_expr,
    );

    // Additive and multiplicative
    parser.led(TokenKind::Plus, BindingPower::Additive, parse_binary_expr);
    parser.led(TokenKind::Dash, BindingPower::Additive, parse_binary_expr);
    parser.led(
        TokenKind::Star,
        BindingPower::Multiplicative,
        parse_binary_expr,
    );
    parser.led(
        TokenKind::Slash,
        BindingPower::Multiplicative,
        parse_binary_expr,
    );
    parser.led(
        TokenKind::Percent,
        BindingPower::Multiplicative,
        parse_binary_expr,
    );

    parser.led(TokenKind::OpenParen, BindingPower::Call, parse_call_expr);

    // Member
    parser.led(TokenKind::Dot, BindingPower::Member, parse_member_expr);
    parser.led(
        TokenKind::OpenBracket,
        BindingPower::Member,
        parse_index_expr,
    );

    // Literals and symbols
    parser.nud(TokenKind::Number, parse_primary_expr);
    parser.nud(TokenKind::Identifier, parse_primary_expr);
    parser.nud(TokenKind::String, parse_primary_expr);
    parser.nud(TokenKind::Dash, parse_prefix_expr);
    parser.nud(TokenKind::OpenParen, parse_grouping_expr);
    parser.nud(TokenKind::New, parse_struct_init_expr);

    // Statements
    parser.stmt(TokenKind::Const, parse_var_decl_stmt);
    parser.stmt(TokenKind::Let, parse_var_decl_stmt);
    parser.stmt(TokenKind::Import, parse_import_stmt);
    parser.stmt(TokenKind::If, parse_if_stmt);
    parser.stmt(TokenKind::OpenCurly, parse_block_stmt);
    parser.stmt(TokenKind::Fn, parse_fn_decl_stmt);
    parser.stmt(TokenKind::Extern, parse_extern_decl_stmt);
    parser.stmt(TokenKind::Return, parse_return_stmt);
    parser.stmt(TokenKind::Struct, parse_struct_decl_stmt);
    parser.stmt(TokenKind::Break, parse_break_stmt);
    parser.stmt(TokenKind::While, parse_while_stmt);
    parser.stmt(TokenKind::Tilde, parse_drop_stmt);
}

// Lookup tables inside parser struct, so it's easier
pub type StmtLookup = HashMap<TokenKind, StmtHandler>;
pub type NUDLookup = HashMap<TokenKind, NUDHandler>;
pub type LEDLookup = HashMap<TokenKind, LEDHandler>;
pub type BPLookup = HashMap<TokenKind, BindingPower>;
