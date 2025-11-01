//! Parser implementation for building the Abstract Syntax Tree.
//!
//! This module contains the main Parser struct and parsing functions.
//! The parser uses a Pratt parser approach with NUD/LED handlers for
//! expression parsing and specialized functions for statement parsing.
//!
//! It maintains lookup tables for:
//! - Statement handlers
//! - NUD (null denotation) handlers for prefix expressions
//! - LED (left denotation) handlers for infix expressions
//! - Binding powers for operator precedence
//! - Type parsing handlers

use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::statements::BlockStmt,
    errors::errors::{Error, ErrorImpl},
    lexer::tokens::{Token, TokenKind},
    Position, Span,
};

use super::{
    lookups::{
        create_token_lookups, BPLookup, BindingPower, LEDHandler, LEDLookup, NUDHandler, NUDLookup,
        StmtHandler, StmtLookup,
    },
    stmt::parse_stmt,
    types::{
        create_token_type_lookups, TypeBPLookup, TypeLEDHandler, TypeLEDLookup, TypeNUDHandler,
        TypeNUDLookup,
    },
};

/// The main parser structure that maintains parsing state.
///
/// This struct holds the token stream and maintains lookup tables for
/// parsing statements, expressions, and types. It tracks the current
/// position in the token stream and provides methods for token consumption.
pub struct Parser {
    /// The list of tokens to parse
    tokens: Vec<Token>,
    /// Current position in the token stream
    pos: i32,
    /// The name of the source file being parsed
    file: Rc<String>,
    /// Lookup table for statement parsing handlers
    stmt_lookup: StmtLookup,
    /// Lookup table for null denotation (prefix) expression handlers
    nud_lookup: NUDLookup,
    /// Lookup table for left denotation (infix) expression handlers
    led_lookup: LEDLookup,
    /// Lookup table for expression binding powers (precedence)
    binding_power_lookup: BPLookup,
    /// Lookup table for type null denotation handlers
    type_nud_lookup: TypeNUDLookup,
    /// Lookup table for type left denotation handlers
    type_led_lookup: TypeLEDLookup,
    /// Lookup table for type binding powers
    type_binding_power_lookup: TypeBPLookup,
    /// Counter for generating unique IDs
    current_id: i32,
}

impl Parser {
    /// Creates a new Parser instance.
    ///
    /// # Arguments
    ///
    /// * `tokens` - Vector of tokens to parse
    /// * `file` - Reference-counted string containing the source file name
    ///
    /// # Returns
    ///
    /// A new Parser instance ready to parse the token stream.
    pub fn new(tokens: Vec<Token>, file: Rc<String>) -> Self {
        Parser {
            tokens,
            pos: 0,
            file,
            stmt_lookup: HashMap::new(),
            nud_lookup: HashMap::new(),
            led_lookup: HashMap::new(),
            binding_power_lookup: HashMap::new(),
            type_nud_lookup: HashMap::new(),
            type_led_lookup: HashMap::new(),
            type_binding_power_lookup: HashMap::new(),
            current_id: 1024, // Give some space for reserved ids
        }
    }

    /// Returns the current token without advancing.
    pub fn current_token(&self) -> &Token {
        self.tokens.get(self.pos as usize).unwrap()
    }

    /// Returns the kind of the current token.
    pub fn current_token_kind(&self) -> TokenKind {
        self.tokens.get(self.pos as usize).unwrap().kind
    }

    /// Advances to the next token and returns the previous token.
    pub fn advance(&mut self) -> &Token {
        self.pos += 1;
        self.tokens.get((self.pos - 1) as usize).unwrap()
    }

    /// Expects a token of the specified kind, with optional custom error.
    ///
    /// # Arguments
    ///
    /// * `expected_kind` - The expected TokenKind
    /// * `error` - Optional custom error to return if expectation fails
    ///
    /// # Returns
    ///
    /// Returns Ok(Token) if the current token matches, otherwise returns an Error.
    pub fn expect_error(
        &mut self,
        expected_kind: TokenKind,
        error: Option<Error>,
    ) -> Result<Token, Error> {
        let token = self.current_token();
        let kind = token.kind;
        if kind != expected_kind {
            match error {
                Some(_) => Err(error.unwrap()),
                None => Err(Error::new(
                    ErrorImpl::UnexpectedToken {
                        token: token.value.clone(),
                    },
                    token.span.start.clone(),
                )),
            }
        } else {
            Ok(self.advance().clone())
        }
    }

    /// Expects a token of the specified kind with default error message.
    ///
    /// # Arguments
    ///
    /// * `expected_kind` - The expected TokenKind
    ///
    /// # Returns
    ///
    /// Returns Ok(Token) if the current token matches, otherwise returns a default Error.
    pub fn expect(&mut self, expected_kind: TokenKind) -> Result<Token, Error> {
        self.expect_error(expected_kind, None)
    }

    /// Checks if there are more tokens to parse.
    ///
    /// # Returns
    ///
    /// Returns true if there are more tokens and the current token is not EOF.
    pub fn has_tokens(&self) -> bool {
        self.pos + 1 < self.tokens.len() as i32 && self.current_token_kind() != TokenKind::EOF
    }

    /// Returns a reference to the statement lookup table.
    pub fn get_stmt_lookup(&self) -> &StmtLookup {
        &self.stmt_lookup
    }

    /// Returns a reference to the NUD (null denotation) lookup table.
    pub fn get_nud_lookup(&self) -> &NUDLookup {
        &self.nud_lookup
    }

    /// Returns a reference to the LED (left denotation) lookup table.
    pub fn get_led_lookup(&self) -> &LEDLookup {
        &self.led_lookup
    }

    /// Returns a reference to the binding power lookup table.
    pub fn get_bp_lookup(&self) -> &BPLookup {
        &self.binding_power_lookup
    }

    /// Returns a reference to the type binding power lookup table.
    pub fn get_type_bp_lookup(&self) -> &BPLookup {
        &self.type_binding_power_lookup
    }

    /// Returns a reference to the type NUD lookup table.
    pub fn get_type_nud_lookup(&self) -> &TypeNUDLookup {
        &self.type_nud_lookup
    }

    /// Returns a reference to the type LED lookup table.
    pub fn get_type_led_lookup(&self) -> &TypeLEDLookup {
        &self.type_led_lookup
    }

    /// Registers a left denotation (infix) handler for a token.
    ///
    /// # Arguments
    ///
    /// * `kind` - The token kind to register
    /// * `binding_power` - The precedence/binding power for this operator
    /// * `led_fn` - The handler function for this infix operator
    pub fn led(&mut self, kind: TokenKind, binding_power: BindingPower, led_fn: LEDHandler) {
        self.binding_power_lookup.insert(kind, binding_power);
        self.led_lookup.insert(kind, led_fn);
    }

    /// Registers a null denotation (prefix) handler for a token.
    ///
    /// # Arguments
    ///
    /// * `kind` - The token kind to register
    /// * `nud_fn` - The handler function for this prefix operator
    pub fn nud(&mut self, kind: TokenKind, nud_fn: NUDHandler) {
        self.binding_power_lookup
            .insert(kind, BindingPower::Primary);
        self.nud_lookup.insert(kind, nud_fn);
    }

    /// Registers a statement handler for a token.
    ///
    /// # Arguments
    ///
    /// * `kind` - The token kind to register
    /// * `stmt_fn` - The handler function for this statement type
    pub fn stmt(&mut self, kind: TokenKind, stmt_fn: StmtHandler) {
        self.binding_power_lookup
            .insert(kind, BindingPower::Default);
        self.stmt_lookup.insert(kind, stmt_fn);
    }

    /// Registers a type left denotation handler.
    ///
    /// # Arguments
    ///
    /// * `kind` - The token kind to register
    /// * `binding_power` - The precedence/binding power for this type operator
    /// * `led_fn` - The handler function for this type operator
    pub fn type_led(
        &mut self,
        kind: TokenKind,
        binding_power: BindingPower,
        led_fn: TypeLEDHandler,
    ) {
        self.type_binding_power_lookup.insert(kind, binding_power);
        self.type_led_lookup.insert(kind, led_fn);
    }

    /// Registers a type null denotation handler.
    ///
    /// # Arguments
    ///
    /// * `kind` - The token kind to register
    /// * `nud_fn` - The handler function for this type
    pub fn type_nud(&mut self, kind: TokenKind, nud_fn: TypeNUDHandler) {
        self.type_binding_power_lookup
            .insert(kind, BindingPower::Primary);
        self.type_nud_lookup.insert(kind, nud_fn);
    }

    /// Advances the internal ID counter and returns the previous value.
    ///
    /// # Returns
    ///
    /// The current ID value before incrementing.
    pub fn advance_id(&mut self) -> i32 {
        let id = self.current_id;
        self.current_id += 1;
        id
    }

    /// Returns the current position in the source file.
    pub fn get_position(&self) -> Position {
        Position(self.pos as u32, Rc::clone(&self.file))
    }
}

/// Parses a stream of tokens into an Abstract Syntax Tree.
///
/// This is the main entry point for parsing. It creates a parser instance,
/// initializes all lookup tables, and parses all statements until EOF.
///
/// # Arguments
///
/// * `tokens` - Vector of tokens to parse
/// * `file` - Reference-counted string containing the source file name
///
/// # Returns
///
/// A tuple containing:
/// - The Parser instance (with state after parsing)
/// - Result containing either the root BlockStmt or an Error
pub fn parse(tokens: Vec<Token>, file: Rc<String>) -> (Parser, Result<BlockStmt, Error>) {
    let mut parser = Parser::new(tokens, Rc::clone(&file));
    create_token_lookups(&mut parser);
    create_token_type_lookups(&mut parser);

    let mut body = vec![];

    while parser.has_tokens() {
        let stmt = parse_stmt(&mut parser);
        if let Ok(stmt) = stmt {
            body.push(stmt);
        } else {
            // parser.advance();
            return (parser, Err(stmt.err().unwrap()));
        }
    }

    let block = Ok(BlockStmt {
        body,
        id: 0,
        span: Span {
            start: Position(0, Rc::clone(&file)),
            end: parser.get_position(),
        },
    });

    (parser, block)
}
