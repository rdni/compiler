use crate::{ast::{ast::{Expr, StmtWrapper, TypeWrapper}, statements::{BlockStmt, ExpressionStmt, FnDeclStmt, IfStmt, ImportStmt, ReturnStmt, StructDeclStmt, VarDeclStmt}, types::{LiteralType, Literals}}, errors::errors::{Error, ErrorImpl}, lexer::tokens::TokenKind, parser::{expr::parse_expr, lookups::BindingPower}, Span};

use super::{parser::Parser, types::parse_type};

pub fn parse_stmt(parser: &mut Parser) -> Result<StmtWrapper, Error> {
    if parser.get_stmt_lookup().contains_key(&parser.current_token_kind()) {
        return parser.get_stmt_lookup().get(&parser.current_token_kind()).unwrap()(parser);
    }

    let expr = parse_expr(parser, BindingPower::Default)?;
    
    parser.expect(TokenKind::Semicolon)?;

    return Ok(StmtWrapper::new(ExpressionStmt {
        span: expr.get_span().clone(),
        expression: expr,
    }));
}

pub fn parse_var_decl_stmt(parser: &mut Parser) -> Result<StmtWrapper, Error> {
    let explicit_type;
    let assigned_value;

    let start_token = parser.advance().clone();
    let is_constant = start_token.kind == TokenKind::Const;

    let error = Error::new(ErrorImpl::UnexpectedTokenDetailed { token: parser.current_token().value.clone(), message: String::from("expected identifier during variable declaration") }, parser.get_position());
    let variable_name = parser.expect_error(TokenKind::Identifier, Some(error))?.value;

    if parser.current_token_kind() == TokenKind::Colon {
        parser.advance();
        explicit_type = Some(parse_type(parser, BindingPower::Default)?);
    } else {
        explicit_type = None;
    }

    if parser.current_token_kind() != TokenKind::Semicolon {
        parser.expect(TokenKind::Assignment)?;
        assigned_value = Some(parse_expr(parser, BindingPower::Default)?);
    } else if explicit_type.is_none() {
        return Err(Error::new(ErrorImpl::UnexpectedTokenDetailed {
            token: parser.current_token().value.clone(),
            message: String::from("expected rhs or explicit type")
        }, parser.get_position()));
    } else {
        assigned_value = None;
    }

    parser.expect(TokenKind::Semicolon)?;

    if is_constant && assigned_value.is_none() {
        return Err(Error::new(ErrorImpl::UnexpectedTokenDetailed { token: parser.current_token().value.clone(), message: String::from("expected rhs in constant definition") }, parser.get_position()));
    }

    Ok(StmtWrapper::new(VarDeclStmt {
        span: Span {
            start: start_token.span.start.clone(),
            end: parser.get_position()
        },
        is_constant,
        identifier: variable_name,
        assigned_value,
        explicit_type
    }))
}

pub fn parse_import_stmt(parser: &mut Parser) -> Result<StmtWrapper, Error> {
    let start = parser.advance().span.start.clone();

    let identifier = parser.expect(TokenKind::Identifier)?.value;

    let from;
    if parser.current_token_kind() == TokenKind::From {
        parser.advance();
        if parser.current_token_kind() == TokenKind::String {
            from = Some(parser.advance().value.clone());
        } else {
            from = Some(parser.expect(TokenKind::Identifier)?.value);
        }
    } else {
        from = None;
    }

    parser.expect(TokenKind::Semicolon)?;

    Ok(StmtWrapper::new(ImportStmt {
        identifier,
        from,
        span: Span {
            start,
            end: parser.get_position()
        }
    }))
}

pub fn parse_if_stmt(parser: &mut Parser) -> Result<StmtWrapper, Error> {
    let start = parser.advance().span.start.clone();

    let condition = parse_expr(parser, BindingPower::Default)?;
    let body = parse_stmt(parser)?;

    let else_body;
    if parser.current_token_kind() == TokenKind::Else {
        parser.advance();
        else_body = Some(parse_stmt(parser)?);
    } else {
        else_body = None;
    }

    Ok(StmtWrapper::new(IfStmt {
        condition,
        then_body: body,
        else_body,
        span: Span {
            start,
            end: parser.get_position()
        }
    }))
}

pub fn parse_block_stmt(parser: &mut Parser) -> Result<StmtWrapper, Error> {
    let start = parser.advance().span.start.clone();

    let mut statements = Vec::new();
    while parser.current_token_kind() != TokenKind::CloseCurly {
        statements.push(parse_stmt(parser)?);
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(StmtWrapper::new(BlockStmt {
        body: statements,
        id: parser.advance_id(),
        span: Span {
            start,
            end: parser.get_position()
        }
    }))
}

pub fn parse_fn_decl_stmt(parser: &mut Parser) -> Result<StmtWrapper, Error> {
    let start = parser.advance().span.start.clone();

    let identifier = parser.expect(TokenKind::Identifier)?.value;

    parser.expect(TokenKind::OpenParen)?;

    let mut parameters = Vec::new();
    while parser.current_token_kind() != TokenKind::CloseParen {
        let name = parser.expect(TokenKind::Identifier)?.value;
        parser.expect(TokenKind::Colon)?;
        let ty = parse_type(parser, BindingPower::Default)?;
        parameters.push((name, ty));

        if parser.current_token_kind() == TokenKind::Comma {
            parser.advance();
        }
    }

    parser.expect(TokenKind::CloseParen)?;

    let return_type;
    if parser.current_token_kind() == TokenKind::Arrow {
        parser.advance();
        return_type = parse_type(parser, BindingPower::Default)?;
    } else {
        return_type = TypeWrapper::new(LiteralType { literal: Literals::Null });
    }

    let body = parse_block_stmt(parser)?;

    let value = StmtWrapper::new(FnDeclStmt {
        span: Span {
            start,
            end: parser.get_position()
        },
        identifier,
        parameters,
        return_type,
        body: body.as_any().downcast_ref::<BlockStmt>().unwrap().clone()
    });
    // println!("{:?}", value);

    Ok(value)
}

pub fn parse_return_stmt(parser: &mut Parser) -> Result<StmtWrapper, Error> {
    let start = parser.advance().span.start.clone();

    let value;
    if parser.current_token_kind() != TokenKind::Semicolon {
        value = Some(parse_expr(parser, BindingPower::Default)?);
    } else {
        value = None;
    }

    parser.expect(TokenKind::Semicolon)?;

    Ok(StmtWrapper::new(ReturnStmt {
        value,
        span: Span {
            start,
            end: parser.get_position()
        }
    }))
}

pub fn parse_struct_decl_stmt(parser: &mut Parser) -> Result<StmtWrapper, Error> {
    let start = parser.advance().span.start.clone();

    let identifier = parser.expect(TokenKind::Identifier)?.value;

    parser.expect(TokenKind::OpenCurly)?;

    let mut properties = Vec::new();
    while parser.current_token_kind() != TokenKind::CloseCurly {
        let name = parser.expect(TokenKind::Identifier)?.value;
        parser.expect(TokenKind::Colon)?;
        let ty = parse_type(parser, BindingPower::Default)?;
        properties.push((name, ty));

        if parser.current_token_kind() == TokenKind::Comma {
            parser.advance();
        }
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(StmtWrapper::new(StructDeclStmt {
        name: identifier,
        fields: properties,
        span: Span {
            start,
            end: parser.get_position()
        }
    }))
}