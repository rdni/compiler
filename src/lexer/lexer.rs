//! Lexer implementation for tokenizing source code.
//!
//! This module contains the main Lexer struct and associated functions
//! for converting source code into tokens. It uses regex patterns to match
//! different token types including:
//!
//! - Keywords (let, const, fn, etc.)
//! - Identifiers and symbols
//! - Numeric literals (integers and floats)
//! - String literals (with escape sequence support)
//! - Operators and punctuation
//! - Comments (skipped during tokenization)
//!
//! The lexer maintains position information for each token to support
//! accurate error reporting in later compilation stages.

use std::rc::Rc;

use regex::Regex;

use crate::{
    errors::errors::{Error, ErrorImpl},
    Position, Span, MK_DEFAULT_HANDLER, MK_TOKEN,
};

use super::tokens::{Token, TokenKind, RESERVED_LOOKUP};

/// Type alias for regex pattern handler functions.
///
/// Handler functions process matched regex patterns and create tokens.
pub type RegexHandler = fn(&mut Lexer, Regex);

/// Represents a regex pattern and its associated handler.
#[derive(Clone)]
pub struct RegexPattern {
    regex: Regex,
    handler: RegexHandler,
}

/// The Lexer struct responsible for tokenizing the source code.
#[derive(Clone)]
pub struct Lexer {
    /// The list of regex patterns to match against the source code.
    patterns: Vec<RegexPattern>,
    /// The list of tokens generated from the source code.
    tokens: Vec<Token>,
    /// The original source code being tokenized.
    source: String,
    /// The current position in the source code.
    pos: i32,
    /// The file name being tokenized.
    file: Rc<String>,
}

impl Lexer {
    /// Creates a new Lexer instance.
    ///
    /// # Arguments
    /// * `source` - The source code to be tokenized.
    /// * `file` - An optional file name for the source code.
    pub fn new(source: String, file: Option<String>) -> Lexer {
        // Check if the file is provided, else use "shell"
        let file_name = if let Some(file) = file {
            Rc::new(file)
        } else {
            Rc::new(String::from("shell"))
        };

        Lexer {
            pos: 0,
            tokens: vec![],
            patterns: vec![
                RegexPattern {
                    regex: Regex::new("[a-zA-Z_][a-zA-Z0-9_]*").unwrap(),
                    handler: symbol_handler,
                },
                RegexPattern {
                    regex: Regex::new("[0-9]+(\\.[0-9]+)?").unwrap(),
                    handler: number_handler,
                },
                RegexPattern {
                    regex: Regex::new("\\s+").unwrap(),
                    handler: skip_handler,
                },
                RegexPattern {
                    regex: Regex::new(r#""([^"\\]|\\.)*""#).unwrap(),
                    handler: string_handler,
                },
                RegexPattern {
                    regex: Regex::new("\\/\\/.*").unwrap(),
                    handler: skip_handler,
                },
                RegexPattern {
                    regex: Regex::new("~").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Tilde, "~"),
                },
                RegexPattern {
                    regex: Regex::new("\\[").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::OpenBracket, "["),
                },
                RegexPattern {
                    regex: Regex::new("\\]").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::CloseBracket, "]"),
                },
                RegexPattern {
                    regex: Regex::new("\\{").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::OpenCurly, "{"),
                },
                RegexPattern {
                    regex: Regex::new("\\}").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::CloseCurly, "}"),
                },
                RegexPattern {
                    regex: Regex::new("\\(").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::OpenParen, "("),
                },
                RegexPattern {
                    regex: Regex::new("\\)").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::CloseParen, ")"),
                },
                RegexPattern {
                    regex: Regex::new("==").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Equals, "=="),
                },
                RegexPattern {
                    regex: Regex::new("!=").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::NotEquals, "!="),
                },
                RegexPattern {
                    regex: Regex::new("!").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Not, "!"),
                },
                RegexPattern {
                    regex: Regex::new("=").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Assignment, "="),
                },
                RegexPattern {
                    regex: Regex::new("<=").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::LessEquals, "<="),
                },
                RegexPattern {
                    regex: Regex::new("<").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Less, "<"),
                },
                RegexPattern {
                    regex: Regex::new(">=").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::GreaterEquals, ">="),
                },
                RegexPattern {
                    regex: Regex::new(">").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Greater, ">"),
                },
                RegexPattern {
                    regex: Regex::new("\\|\\|").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Or, "||"),
                },
                RegexPattern {
                    regex: Regex::new("&&").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::And, "&&"),
                },
                // RegexPattern { regex: Regex::new("\\.\\.").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::DotDot, "..")},
                RegexPattern {
                    regex: Regex::new("\\.\\.\\.").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Ellipsis, "..."),
                },
                RegexPattern {
                    regex: Regex::new("\\.").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Dot, "."),
                },
                RegexPattern {
                    regex: Regex::new(";").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Semicolon, ";"),
                },
                RegexPattern {
                    regex: Regex::new(":").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Colon, ":"),
                },
                RegexPattern {
                    regex: Regex::new("\\?").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Question, "?"),
                },
                RegexPattern {
                    regex: Regex::new(",").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Comma, ","),
                },
                RegexPattern {
                    regex: Regex::new("\\+\\+").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::PlusPlus, "++"),
                },
                RegexPattern {
                    regex: Regex::new("->").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Arrow, "->"),
                },
                RegexPattern {
                    regex: Regex::new("--").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::MinusMinus, "--"),
                },
                RegexPattern {
                    regex: Regex::new("\\+=").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::PlusEquals, "+="),
                },
                RegexPattern {
                    regex: Regex::new("-=").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::MinusEquals, "-="),
                },
                RegexPattern {
                    regex: Regex::new("\\+").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Plus, "+"),
                },
                RegexPattern {
                    regex: Regex::new("-").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Dash, "-"),
                },
                RegexPattern {
                    regex: Regex::new("/").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Slash, "/"),
                },
                RegexPattern {
                    regex: Regex::new("\\*").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Star, "*"),
                },
                RegexPattern {
                    regex: Regex::new("%").unwrap(),
                    handler: MK_DEFAULT_HANDLER!(TokenKind::Percent, "%"),
                },
            ],
            source,
            file: file_name,
        }
    }

    /// Advances the current position by `n` characters.
    pub fn advance_n(&mut self, n: i32) {
        self.pos += n;
    }

    /// Pushes a new token to the tokens list.
    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    /// Returns the character at the current position.
    pub fn at(&self) -> char {
        self.source.as_bytes()[self.pos as usize] as char
    }

    /// Returns the remaining characters from the current position as a vector of chars.
    pub fn remainder(&self) -> Vec<char> {
        (self.source.as_bytes()[(self.pos as usize)..])
            .iter()
            .map(|x| *x as char)
            .collect::<Vec<char>>()
    }

    /// Checks if the lexer has reached the end of the source code.
    pub fn at_eof(&self) -> bool {
        self.pos as usize >= self.source.len()
    }
}

/// Handler for number tokens.
fn number_handler(lexer: &mut Lexer, regex: Regex) {
    let remaining = &lexer.remainder().iter().collect::<String>();
    let matched = regex.find(remaining).unwrap().as_str().to_string();

    lexer.push(MK_TOKEN!(
        TokenKind::Number,
        matched.clone(),
        Span {
            start: Position(lexer.pos as u32, Rc::clone(&lexer.file)),
            end: Position(
                (lexer.pos + matched.len() as i32) as u32,
                Rc::clone(&lexer.file)
            )
        }
    ));
    lexer.advance_n(matched.len() as i32);
}

/// Handler to skip tokens (e.g., whitespace, comments).
fn skip_handler(lexer: &mut Lexer, regex: Regex) {
    let remaining = &lexer.remainder().iter().collect::<String>();
    let matched = regex.find(remaining).unwrap().end();
    lexer.advance_n(matched as i32);
}

/// Handler for string tokens, processes escape sequences.
fn string_handler(lexer: &mut Lexer, regex: Regex) {
    // Find the string literal in the remaining input
    let binding = lexer.remainder().iter().collect::<String>();
    let matched = regex.find(&binding).unwrap();
    let mut string_literal = lexer.remainder()[(matched.start() + 1)..(matched.end() - 1)]
        .iter()
        .collect::<String>();

    // Advance the lexer position past the entire string literal including quotes
    lexer.advance_n(string_literal.len() as i32 + 2);

    let mut result = String::new();
    let mut chars = string_literal.chars().peekable();

    // Loop through each character, push if not escape, else handle escape sequence
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next_ch) = chars.peek() {
                match next_ch {
                    'n' => {
                        result.push('\n');
                        chars.next();
                    }
                    't' => {
                        result.push('\t');
                        chars.next();
                    }
                    '\\' => {
                        result.push('\\');
                        chars.next();
                    }
                    'r' => {
                        result.push('\r');
                        chars.next();
                    }
                    '"' => {
                        result.push('"');
                        chars.next();
                    }
                    '0' => {
                        result.push('\0');
                        chars.next();
                    }
                    'x' => {
                        let mut hex = String::new();
                        chars.next();

                        for _ in 0..2 {
                            if let Some(ch) = chars.peek() {
                                if ch.is_ascii_hexdigit() {
                                    hex.push(*ch);
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                        }

                        result.push(u8::from_str_radix(&hex, 16).unwrap() as char);
                    }
                    _ => {
                        result.push(ch); // Keep the backslash
                    }
                }
            } else {
                result.push(ch); // Keep the lone backslash
            }
        } else {
            result.push(ch); // Keep non-escape characters
        }
    }

    string_literal = result;

    lexer.push(MK_TOKEN!(
        TokenKind::String,
        string_literal.clone(),
        Span {
            start: Position(lexer.pos as u32, Rc::clone(&lexer.file)),
            end: Position(
                (lexer.pos + string_literal.len() as i32) as u32,
                Rc::clone(&lexer.file)
            )
        }
    ));
}

/// Handler for symbol tokens (identifiers and reserved keywords).
fn symbol_handler(lexer: &mut Lexer, regex: Regex) {
    // Find the symbol in the remaining input
    let binding = lexer.remainder().iter().collect::<String>();
    let value = regex.find(&binding).unwrap();

    // Check if the symbol is a reserved keyword
    if let Some(kind) = RESERVED_LOOKUP.get(value.as_str()) {
        lexer.push(MK_TOKEN!(
            *kind,
            String::from(value.as_str()),
            Span {
                start: Position(lexer.pos as u32, Rc::clone(&lexer.file)),
                end: Position(
                    (lexer.pos + value.len() as i32) as u32,
                    Rc::clone(&lexer.file)
                )
            }
        ));
    } else {
        lexer.push(MK_TOKEN!(
            TokenKind::Identifier,
            String::from(value.as_str()),
            Span {
                start: Position(lexer.pos as u32, Rc::clone(&lexer.file)),
                end: Position(
                    (lexer.pos + value.len() as i32) as u32,
                    Rc::clone(&lexer.file)
                )
            }
        ));
    }

    lexer.advance_n(value.len() as i32);
}

/// Tokenizes the given source code into a vector of tokens.
///
/// # Arguments
/// * `source` - The source code to be tokenized.
/// * `file` - An optional file name for the source code.
///
/// # Returns
/// A Result containing a vector of tokens or an Error.
pub fn tokenize(source: String, file: Option<String>) -> Result<Vec<Token>, Error> {
    let mut lexer = Lexer::new(source, file);

    // Main lexing loop
    while !lexer.at_eof() {
        let mut matched = false;

        for pattern in lexer.clone().patterns.iter() {
            let string = &lexer.remainder().iter().collect::<String>();
            let match_here = pattern.regex.find(string);

            if match_here.is_some() && match_here.unwrap().start() == 0 {
                (pattern.handler)(&mut lexer, pattern.regex.clone());
                matched = true;
                break;
            }
        }

        // If no patterns matched, return an error
        if !matched {
            return Err(Error::new(
                ErrorImpl::UnrecognisedToken {
                    token: lexer.at().to_string(),
                },
                Position(lexer.pos as u32, Rc::clone(&lexer.file)),
            ));
        }
    }

    // Push the EOF token
    lexer.push(MK_TOKEN!(
        TokenKind::EOF,
        String::from("EOF"),
        Span {
            start: Position(lexer.pos as u32, Rc::clone(&lexer.file)),
            end: Position(lexer.pos as u32, Rc::clone(&lexer.file))
        }
    ));
    Ok(lexer.tokens)
}
