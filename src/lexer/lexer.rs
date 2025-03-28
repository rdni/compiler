use std::rc::Rc;

use regex::Regex;

use crate::{errors::errors::{Error, ErrorImpl}, Position, Span, MK_DEFAULT_HANDLER, MK_TOKEN};

use super::tokens::{Token, TokenKind, RESERVED_LOOKUP};

pub type RegexHandler = fn(&mut Lexer, Regex);

#[derive(Clone)]
pub struct RegexPattern {
    regex: Regex,
    handler: RegexHandler
}

#[derive(Clone)]
pub struct Lexer {
    patterns: Vec<RegexPattern>,
    tokens: Vec<Token>,
    source: String,
    pos: i32,
    file: Rc<String>,
}

impl Lexer {
    pub fn new(source: String, file: Option<String>) -> Lexer {
        let file_name = if let Some(file) = file {
            Rc::new(file)
        } else {
            Rc::new(String::from("shell"))
        };

        Lexer {
            pos: 0,
            tokens: vec![],
            patterns: vec![
                RegexPattern { regex: Regex::new("[a-zA-Z_][a-zA-Z0-9_]*").unwrap(), handler: symbol_handler},
                RegexPattern { regex: Regex::new("[0-9]+(\\.[0-9]+)?").unwrap(), handler: number_handler},
                RegexPattern { regex: Regex::new("\\s+").unwrap(), handler: skip_handler},
                RegexPattern { regex: Regex::new("\"[^\"]*\"").unwrap(), handler: string_handler},
                RegexPattern { regex: Regex::new("\\/\\/.*").unwrap(), handler: skip_handler},
                RegexPattern { regex: Regex::new("\\[").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::OpenBracket, "[")},
                RegexPattern { regex: Regex::new("\\]").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::CloseBracket, "]")},
                RegexPattern { regex: Regex::new("\\{").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::OpenCurly, "{")},
                RegexPattern { regex: Regex::new("\\}").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::CloseCurly, "}")},
                RegexPattern { regex: Regex::new("\\(").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::OpenParen, "(")},
                RegexPattern { regex: Regex::new("\\)").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::CloseParen, ")")},
                RegexPattern { regex: Regex::new("==").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Equals, "==")},
                RegexPattern { regex: Regex::new("!=").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::NotEquals, "!=")},
                RegexPattern { regex: Regex::new("!").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Not, "!")},
                RegexPattern { regex: Regex::new("=").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Assignment, "=")},
                RegexPattern { regex: Regex::new("<=").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::LessEquals, "<=")},
                RegexPattern { regex: Regex::new("<").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Less, "<")},
                RegexPattern { regex: Regex::new(">=").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::GreaterEquals, ">=")},
                RegexPattern { regex: Regex::new(">").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Greater, ">")},
                RegexPattern { regex: Regex::new("\\|\\|").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Or, "||")},
                RegexPattern { regex: Regex::new("&&").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::And, "&&")},
                // RegexPattern { regex: Regex::new("\\.\\.").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::DotDot, "..")},
                RegexPattern { regex: Regex::new("\\.").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Dot, ".")},
                RegexPattern { regex: Regex::new(";").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Semicolon, ";")},
                RegexPattern { regex: Regex::new(":").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Colon, ":")},
                RegexPattern { regex: Regex::new("\\?").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Question, "?")},
                RegexPattern { regex: Regex::new(",").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Comma, ",")},
                RegexPattern { regex: Regex::new("\\+\\+").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::PlusPlus, "++")},
                RegexPattern { regex: Regex::new("->").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Arrow, "->")},
                RegexPattern { regex: Regex::new("--").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::MinusMinus, "--")},
                RegexPattern { regex: Regex::new("\\+=").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::PlusEquals, "+=")},
                RegexPattern { regex: Regex::new("-=").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::MinusEquals, "-=")},
                RegexPattern { regex: Regex::new("\\+").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Plus, "+")},
                RegexPattern { regex: Regex::new("-").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Dash, "-")},
                RegexPattern { regex: Regex::new("/").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Slash, "/")},
                RegexPattern { regex: Regex::new("\\*").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Star, "*")},
                RegexPattern { regex: Regex::new("%").unwrap(), handler: MK_DEFAULT_HANDLER!(TokenKind::Percent, "%")}
            ],
            source,
            file: file_name,
        }
    }



    pub fn advance_n(&mut self, n: i32) {
        self.pos += n;
    }

    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn at(&self) -> char {
        self.source.as_bytes()[self.pos as usize] as char
    }

    pub fn remainder(&self) -> Vec<char> {
        (self.source.as_bytes()[(self.pos as usize)..]).iter().map(|x| {*x as char}).collect::<Vec<char>>()
    }

    pub fn at_eof(&self) -> bool {
        self.pos as usize >= self.source.len()
    }
}

fn number_handler(lexer: &mut Lexer, regex: Regex) {
    let remaining = &lexer.remainder().iter().collect::<String>();
    let matched = regex.find(remaining).unwrap().as_str().to_string();

    lexer.push(MK_TOKEN!(TokenKind::Number, matched.clone(), Span { start: Position(lexer.pos as u32, Rc::clone(&lexer.file)), end: Position((lexer.pos + matched.len() as i32) as u32, Rc::clone(&lexer.file)) }));
    lexer.advance_n(matched.len() as i32);
}

fn skip_handler(lexer: &mut Lexer, regex: Regex) {
    let remaining = &lexer.remainder().iter().collect::<String>();
    let matched = regex.find(remaining).unwrap().end();
    lexer.advance_n(matched as i32);
}

fn string_handler(lexer: &mut Lexer, regex: Regex) {
    let binding = lexer.remainder().iter().collect::<String>();
    let matched = regex.find(&binding).unwrap();
    let mut string_literal = lexer.remainder()[(matched.start()+1)..(matched.end()-1)].iter().collect::<String>();

    lexer.advance_n(string_literal.len() as i32 + 2);

    let mut result = String::new();
    let mut chars = string_literal.chars().peekable();

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

    lexer.push(MK_TOKEN!(TokenKind::String, string_literal.clone(), Span { start: Position(lexer.pos as u32, Rc::clone(&lexer.file)), end: Position((lexer.pos + string_literal.len() as i32) as u32, Rc::clone(&lexer.file)) }));
}

fn symbol_handler(lexer: &mut Lexer, regex: Regex) {
    let binding = lexer.remainder().iter().collect::<String>();
    let value = regex.find(&binding).unwrap();

    if let Some(kind) = RESERVED_LOOKUP.get(value.as_str()) {
        lexer.push(MK_TOKEN!(*kind, String::from(value.as_str()), Span { start: Position(lexer.pos as u32, Rc::clone(&lexer.file)), end: Position((lexer.pos + value.len() as i32) as u32, Rc::clone(&lexer.file)) }));
    } else {
        lexer.push(MK_TOKEN!(TokenKind::Identifier, String::from(value.as_str()), Span { start: Position(lexer.pos as u32, Rc::clone(&lexer.file)), end: Position((lexer.pos + value.len() as i32) as u32, Rc::clone(&lexer.file)) }));
    }

    lexer.advance_n(value.len() as i32);
}

pub fn tokenize(source: String, file: Option<String>) -> Result<Vec<Token>, Error> {
    let mut lex = Lexer::new(source, file);

    while !lex.at_eof() {
        let mut matched = false;
        
        for pattern in lex.clone().patterns.iter() {
            let string = &lex.remainder().iter().collect::<String>();
            let match_here = pattern.regex.find(string);

            if match_here.is_some() && match_here.unwrap().start() == 0 {
                (pattern.handler)(&mut lex, pattern.regex.clone());
                matched = true;
                break;
            }
        }

        if !matched {
            return Err(Error::new(ErrorImpl::UnrecognisedToken { token: lex.at().to_string() }, Position(lex.pos as u32, Rc::clone(&lex.file))));
        }
    }

    lex.push(MK_TOKEN!(TokenKind::EOF, String::from("EOF"), Span { start: Position(lex.pos as u32, Rc::clone(&lex.file)), end: Position(lex.pos as u32, Rc::clone(&lex.file)) }));
    Ok(lex.tokens)
}