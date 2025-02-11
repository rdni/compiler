use crate::{ast::{ast::{Expr, ExprWrapper}, expressions::{AssignmentExpr, BinaryExpr, CallExpr, NumberExpr, PrefixExpr, StringExpr, StructInitExpr, SymbolExpr}}, errors::errors::{Error, ErrorImpl}, lexer::tokens::TokenKind, Span};

use super::{lookups::BindingPower, parser::Parser};

pub fn parse_expr(parser: &mut Parser, bp: BindingPower) -> Result<ExprWrapper, Error> {
    // First parse NUD
    let token_kind = parser.current_token_kind();
    if !parser.get_nud_lookup().contains_key(&token_kind) {
        return Err(Error::new(ErrorImpl::UnexpectedToken { token: parser.current_token().value.clone() }, parser.get_position()));
    }

    let mut left = parser.get_nud_lookup().get(&token_kind).unwrap()(parser)?;

    // While LED and current BP is less than BP of current token, continue parsing lhs
    while *parser.get_bp_lookup().get(&parser.current_token_kind()).unwrap_or(&BindingPower::Default) > bp {
        let token_kind = parser.current_token_kind();
        if !parser.get_led_lookup().contains_key(&token_kind) {
            return Err(Error::new(ErrorImpl::UnexpectedToken { token: format!("{:?}", left) }, left.get_span().start.clone()));
        }

        left = parser.get_led_lookup().get(&token_kind).unwrap()(parser, left, *parser.get_bp_lookup().get(&parser.current_token_kind()).unwrap())?;
    }

    return Ok(left);
}

pub fn parse_primary_expr(parser: &mut Parser) -> Result<ExprWrapper, Error> {
    match parser.current_token_kind() {
        TokenKind::Number => {
            let result = parser.current_token().value.parse();
            
            if result.is_err() {
                Err(Error::new(ErrorImpl::NumberParseError { token: parser.current_token().value.clone() }, parser.get_position()))
            } else {
                Ok(ExprWrapper::new(NumberExpr { value: result.unwrap(), span: parser.advance().span.clone() }))
            }
        },
        TokenKind::Identifier => {
            Ok(ExprWrapper::new(SymbolExpr { value: parser.current_token().value.clone(), span: parser.advance().span.clone() }))
        },
        TokenKind::String => {
            Ok(ExprWrapper::new(StringExpr { value: parser.current_token().value.clone(), span: parser.advance().span.clone() }))
        }
        _ => {
            Err(Error::new(ErrorImpl::UnexpectedToken { token: parser.current_token().value.clone() }, parser.get_position()))
        }
    }
}

pub fn parse_binary_expr(parser: &mut Parser, left: ExprWrapper, bp: BindingPower) -> Result<ExprWrapper, Error> {
    let operator_token = parser.advance().clone();

    let right = parse_expr(parser, bp)?;

    return Ok(ExprWrapper::new(BinaryExpr {
        span: Span {
            start: left.get_span().start.clone(),
            end: right.get_span().end.clone()
        },
        left,
        operator: operator_token.clone(),
        right,
    }));
}

pub fn parse_prefix_expr(parser: &mut Parser) -> Result<ExprWrapper, Error> {
    let operator_token = parser.advance().clone();
    let rhs = parse_expr(parser, BindingPower::Default)?;

    return Ok(ExprWrapper::new(PrefixExpr {
        span: Span {
            start: operator_token.span.start.clone(),
            end: rhs.get_span().end.clone()
        },
        operator: operator_token,
        right_expr: rhs
    }));
}

pub fn parse_assignment_expr(parser: &mut Parser, left: ExprWrapper, bp: BindingPower) -> Result<ExprWrapper, Error> {
    let operator_token = parser.advance().clone();
    let rhs = parse_expr(parser, bp)?;

    return Ok(ExprWrapper::new(AssignmentExpr {
        span: Span {
            start: left.get_span().start.clone(),
            end: rhs.get_span().end.clone()
        },
        operator: operator_token,
        value: rhs,
        assignee: left
    }))
}

pub fn parse_grouping_expr(parser: &mut Parser) -> Result<ExprWrapper, Error> {
    parser.advance();
    let expr = parse_expr(parser, BindingPower::Default)?;
    parser.advance();
    
    return Ok(expr);
}

pub fn parse_call_expr(parser: &mut Parser, left: ExprWrapper, _bp: BindingPower) -> Result<ExprWrapper, Error> {
    parser.advance();

    let mut args = vec![];

    while parser.current_token_kind() != TokenKind::CloseParen {
        if parser.current_token_kind() == TokenKind::Comma {
            parser.advance();
            continue;
        } else {
            args.push(parse_expr(parser, BindingPower::Default)?);
        }
    }

    parser.expect(TokenKind::CloseParen)?;

    Ok(ExprWrapper::new(CallExpr {
        span: Span {
            start: left.get_span().start.clone(),
            end: parser.get_position()
        },
        callee: left,
        arguments: args
    }))
}

pub fn parse_member_expr(parser: &mut Parser, left: ExprWrapper, _bp: BindingPower) -> Result<ExprWrapper, Error> {
    let operator = parser.advance().clone();
    let member = parse_expr(parser, BindingPower::Primary)?;

    return Ok(ExprWrapper::new(BinaryExpr {
        span: Span {
            start: left.get_span().start.clone(),
            end: parser.get_position()
        },
        left,
        operator,
        right: member,
    }));
}

pub fn parse_struct_init_expr(parser: &mut Parser) -> Result<ExprWrapper, Error> {
    // new Test { field1: 1, field2: 2 }
    let start_pos = parser.advance().span.start.clone();

    let struct_name = parser.expect(TokenKind::Identifier)?.value;

    parser.expect(TokenKind::OpenCurly)?;

    let mut fields = vec![];

    while parser.current_token_kind() != TokenKind::CloseCurly {
        let field_name = parser.expect(TokenKind::Identifier)?.value;
        parser.expect(TokenKind::Colon)?;
        let field_value = parse_expr(parser, BindingPower::Default)?;

        fields.push((field_name, field_value));

        if parser.current_token_kind() == TokenKind::Comma {
            parser.advance();
        }
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(ExprWrapper::new(StructInitExpr {
        name: struct_name,
        fields,
        span: Span {
            start: start_pos,
            end: parser.get_position()
        }
    }))
}