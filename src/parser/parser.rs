use std::{collections::HashMap, rc::Rc};

use crate::{ast::statements::BlockStmt, errors::errors::{Error, ErrorImpl}, lexer::tokens::{Token, TokenKind}, Position, Span};

use super::{lookups::{create_token_lookups, BPLookup, BindingPower, LEDHandler, LEDLookup, NUDHandler, NUDLookup, StmtHandler, StmtLookup}, stmt::parse_stmt, types::{create_token_type_lookups, TypeBPLookup, TypeLEDHandler, TypeLEDLookup, TypeNUDHandler, TypeNUDLookup}};

pub struct Parser {
    tokens: Vec<Token>,
    pos: i32,
    file: Rc<String>,
    stmt_lookup: StmtLookup,
    nud_lookup: NUDLookup,
    led_lookup: LEDLookup,
    binding_power_lookup: BPLookup,
    type_nud_lookup: TypeNUDLookup,
    type_led_lookup: TypeLEDLookup,
    type_binding_power_lookup: TypeBPLookup,
    current_id: i32
}

impl Parser {
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
            current_id: 1024 // Give some space for reserved ids
        }
    }

    pub fn current_token(&self) -> &Token {
        self.tokens.get(self.pos as usize).unwrap()
    }

    pub fn current_token_kind(&self) -> TokenKind {
        self.tokens.get(self.pos as usize).unwrap().kind
    }

    pub fn advance(&mut self) -> &Token {
        self.pos += 1;
        self.tokens.get((self.pos - 1) as usize).unwrap()
    }

    pub fn expect_error(&mut self, expected_kind: TokenKind, error: Option<Error>) -> Result<Token, Error> {
        let token = self.current_token();
        let kind = token.kind;
        if kind != expected_kind {
            match error {
                Some(_) => {
                    Err(error.unwrap())
                },
                None => {
                    Err(Error::new(ErrorImpl::UnexpectedToken { token: token.value.clone() }, token.span.start.clone()))
                }
            }
        } else {
            Ok(self.advance().clone())
        }
    }

    pub fn expect(&mut self, expected_kind: TokenKind) -> Result<Token, Error> {
        self.expect_error(expected_kind, None)
    }

    pub fn has_tokens(&self) -> bool {
        self.pos + 1 < self.tokens.len() as i32 && self.current_token_kind() != TokenKind::EOF
    }

    pub fn get_stmt_lookup(&self) -> &StmtLookup {
        &self.stmt_lookup
    }

    pub fn get_nud_lookup(&self) -> &NUDLookup {
        &self.nud_lookup
    }

    pub fn get_led_lookup(&self) -> &LEDLookup {
        &self.led_lookup
    }

    pub fn get_bp_lookup(&self) -> &BPLookup {
        &self.binding_power_lookup
    }

    pub fn get_type_bp_lookup(&self) -> &BPLookup {
        &self.type_binding_power_lookup
    }

    pub fn get_type_nud_lookup(&self) -> &TypeNUDLookup {
        &self.type_nud_lookup
    }

    pub fn get_type_led_lookup(&self) -> &TypeLEDLookup {
        &self.type_led_lookup
    }

    pub fn led(&mut self, kind: TokenKind, binding_power: BindingPower, led_fn: LEDHandler) {
        self.binding_power_lookup.insert(kind, binding_power);
        self.led_lookup.insert(kind, led_fn);
    }

    pub fn nud(&mut self, kind: TokenKind, nud_fn: NUDHandler) {
        self.binding_power_lookup.insert(kind, BindingPower::Primary);
        self.nud_lookup.insert(kind, nud_fn);
    }

    pub fn stmt(&mut self, kind: TokenKind, stmt_fn: StmtHandler) {
        self.binding_power_lookup.insert(kind, BindingPower::Default);
        self.stmt_lookup.insert(kind, stmt_fn);
    }

    pub fn type_led(&mut self, kind: TokenKind, binding_power: BindingPower, led_fn: TypeLEDHandler) {
        self.type_binding_power_lookup.insert(kind, binding_power);
        self.type_led_lookup.insert(kind, led_fn);
    }

    pub fn type_nud(&mut self, kind: TokenKind, nud_fn: TypeNUDHandler) {
        self.type_binding_power_lookup.insert(kind, BindingPower::Primary);
        self.type_nud_lookup.insert(kind, nud_fn);
    }

    pub fn advance_id(&mut self) -> i32 {
        let id = self.current_id;
        self.current_id += 1;
        id
    }

    pub fn get_position(&self) -> Position {
        Position(self.pos as u32, Rc::clone(&self.file))
    }
}

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
            println!("Error");
            // parser.advance();
            return (parser, Err(stmt.err().unwrap()));
        }
        // Ignore errors until everything parsed
    }

    let block = Ok(BlockStmt {
        body,
        id: 0,
        span: Span {
            start: Position(0, Rc::clone(&file)),
            end: parser.get_position()
        }
    });

    (parser, block)
}