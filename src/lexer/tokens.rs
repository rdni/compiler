use lazy_static::lazy_static;
use std::{collections::HashMap, fmt::Display};

use crate::Span;

lazy_static! {
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

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token {{\nkind: {},\nvalue: {}}}", self.kind, self.value)
    }
}

impl Token {
    fn is_one_of_many(&self, tokens: Vec<TokenKind>) -> bool {
        for token in tokens {
            if token == self.kind {
                return true;
            }
        }

        false
    }

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
