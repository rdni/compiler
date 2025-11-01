//! Type parsing implementation.
//!
//! This module handles parsing of type annotations and type expressions.
//! It supports:
//!
//! - Basic types (identifiers)
//! - Array types
//! - Function types
//! - Struct types
//!
//! Similar to expression parsing, it uses NUD/LED handlers with
//! binding powers for parsing complex type expressions.

use std::collections::HashMap;

use crate::{
    ast::{
        ast::TypeWrapper,
        types::{ArrayType, SymbolType},
    },
    errors::errors::{Error, ErrorImpl},
    lexer::tokens::TokenKind,
};

use super::{lookups::BindingPower, parser::Parser};

/// Type alias for type null denotation handler functions.
pub type TypeNUDHandler = fn(&mut Parser) -> Result<TypeWrapper, Error>;

/// Type alias for type left denotation handler functions.
pub type TypeLEDHandler = fn(&mut Parser, TypeWrapper, BindingPower) -> Result<TypeWrapper, Error>;

/// Type alias for type NUD lookup table.
pub type TypeNUDLookup = HashMap<TokenKind, TypeNUDHandler>;

/// Type alias for type LED lookup table.
pub type TypeLEDLookup = HashMap<TokenKind, TypeLEDHandler>;

/// Type alias for type binding power lookup table.
pub type TypeBPLookup = HashMap<TokenKind, BindingPower>;

/// Initializes the type parsing lookup tables.
///
/// Registers NUD and LED handlers for parsing type expressions.
///
/// # Arguments
///
/// * `parser` - Mutable reference to the parser to initialize
pub fn create_token_type_lookups(parser: &mut Parser) {
    parser.type_nud(TokenKind::Identifier, parse_symbol_type);
    parser.type_led(TokenKind::OpenBracket, BindingPower::Call, parse_array_type);
}

// pub fn parse_generic_type(parser: &mut Parser) -> Result<TypeWrapper, ()> {
//     parser.expect(TokenKind::Less);
//     let generic = parser.expect(TokenKind::Identifier);

//     if parser.current_token_kind() == TokenKind::Comma {

//     }
// }

pub fn parse_symbol_type(parser: &mut Parser) -> Result<TypeWrapper, Error> {
    let token = parser.expect(TokenKind::Identifier)?;
    Ok(TypeWrapper::new(SymbolType {
        name: token.value.clone(),
        position: token.span.start.clone(),
    }))
}

pub fn parse_array_type(
    parser: &mut Parser,
    left: TypeWrapper,
    _bp: BindingPower,
) -> Result<TypeWrapper, Error> {
    parser.expect(TokenKind::OpenBracket)?;
    parser.expect(TokenKind::CloseBracket)?;

    Ok(TypeWrapper::new(ArrayType { underlying: left }))
}

pub fn parse_type(parser: &mut Parser, bp: BindingPower) -> Result<TypeWrapper, Error> {
    // First parse NUD
    let token_kind = parser.current_token_kind();
    if !parser.get_type_nud_lookup().contains_key(&token_kind) {
        return Err(Error::new(
            ErrorImpl::UnexpectedToken {
                token: parser.current_token().value.clone(),
            },
            parser.get_position(),
        ));
    }

    let mut left = parser.get_type_nud_lookup().get(&token_kind).unwrap()(parser)?;

    // While LED and current BP is less than BP of current token, continue parsing lhs
    while *parser
        .get_type_bp_lookup()
        .get(&parser.current_token_kind())
        .unwrap_or(&BindingPower::Default)
        > bp
    {
        let token_kind = parser.current_token_kind();
        if !parser.get_type_led_lookup().contains_key(&token_kind) {
            return Err(Error::new(
                ErrorImpl::UnexpectedToken {
                    token: parser.current_token().value.clone(),
                },
                parser.get_position(),
            ));
        }

        left = parser.get_type_led_lookup().get(&token_kind).unwrap()(
            parser,
            left,
            *parser
                .get_type_bp_lookup()
                .get(&parser.current_token_kind())
                .unwrap(),
        )?;
    }

    Ok(left)
}
