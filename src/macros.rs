#[macro_export]
macro_rules! MK_TOKEN {
    ($kind:expr, $value:expr, $span:expr) => {
        Token {
            kind: $kind,
            value: $value,
            span: $span
        }
    };
}

#[macro_export]
macro_rules! MK_DEFAULT_HANDLER {
    ($kind:expr, $value:literal) => {
        |lexer: &mut Lexer, _regex: Regex| {
            lexer.push(MK_TOKEN!($kind, String::from($value), Span {
                start: Position(lexer.pos as u32, Rc::clone(&lexer.file)),
                end: Position((lexer.pos + $value.len() as i32) as u32, Rc::clone(&lexer.file))
            }));
            lexer.advance_n($value.len().try_into().unwrap());
        }
    };
}