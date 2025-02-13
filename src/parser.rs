use chumsky::prelude::*;

use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    // Integer literal
    Int(isize),
    Ident(String),

    // Unary minus
    Neg(Box<Expr>),

    // Binary operators
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    // Function call
    Call(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum Stmt<'a> {
    FunctionDeclaration {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt<'a>>,
    },
    VariableDeclaration {
        name: String,
        value: Box<Expr>,
    },
    VariableAssignment {
        name: &'a str,
        value: Box<Expr>,
    },
    Return(Box<Expr>),
}

// TODO: Replace this macro with a type alias once that feature lands on stable Rust
macro_rules! parser {
    ($from:ty => $to:ty) => {
        impl chumsky::Parser<$from, $to, Error = chumsky::error::Simple<$from>>
    };
}

/// An identifier, such as `foo` or `bar`
fn ident<'a>() -> parser!(Token<'a> => Expr) {
    select!(|_span| Token::Ident(ident) => Expr::Ident(ident.to_owned()))
}

/// Variable creator, such as `let foo = 4`
fn var_declaration<'a>() -> parser!(Token<'a> => Stmt<'a>) {
    let name = ident();

    just(Token::Ident("let"))
        .ignore_then(name)
        .then_ignore(just(Token::Equals))
        .then(expr())
        .map(|(name, value)| {
            let Expr::Ident(var_name) = name else {
                unreachable!()
            };

            Stmt::VariableDeclaration {
                name: var_name,
                value: Box::new(value),
            }
        })
}

/// Variable mutation, such as `let foo = 4`
fn var_mutation<'a>() -> parser!(Token<'a> => Stmt<'a>) {
    let name = ident();

    name.ignore_then(just(Token::Equals))
        .then(expr())
        .map(|(name, value)| {
            let Token::Ident(var_name) = name else {
                unreachable!()
            };

            Stmt::VariableAssignment {
                name: var_name,
                value: Box::new(value),
            }
        })
}

fn expr<'a>() -> parser!(Token<'a> => Expr) {
    recursive(|expr| {
        let atom = {
            let parenthesized = expr
                .clone()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

            let integer = select!(|_span| Token::Int(n) => Expr::Int(n));
            let ident = select!(|_span| Token::Ident(n) => Expr::Ident(n.to_owned()));

            let call = ident
                .then(
                    expr.repeated()
                        .separated_by(just(Token::Comma))
                        .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
                )
                .map(|(name, params)| {
                    let Expr::Ident(func_name) = name else {
                        unreachable!()
                    };

                    Expr::Call(func_name, params.into_iter().flatten().collect())
                });

            call.or(parenthesized).or(integer).or(ident)
        };

        let unary = just(Token::Minus)
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                just(Token::Multiply)
                    .or(just(Token::Divide))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| match op {
                Token::Multiply => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                Token::Divide => Expr::Div(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            });

        let sum = product
            .clone()
            .then(
                just(Token::Plus)
                    .or(just(Token::Minus))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| match op {
                Token::Plus => Expr::Add(Box::new(lhs), Box::new(rhs)),
                Token::Minus => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            });

        sum
    })
}

/// Accepts a parser for statements valid inside of function body
fn function<'a>(stmt: parser!(Token<'a> => Stmt<'a>)) -> parser!(Token<'a> => Stmt<'a>) {
    let name = ident();

    let parameters = ident()
        .repeated()
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

    // functions have an additional statement
    let ret = just(Token::Ident("return"))
        .ignore_then(expr())
        .map(|ret| Stmt::Return(Box::new(ret)));

    let body = stmt
        .or(ret)
        .repeated()
        .separated_by(just(Token::Semicolon))
        .delimited_by(just(Token::OpenCurly), just(Token::CloseCurly));

    just(Token::Ident("fn"))
        .ignore_then(name)
        .then(parameters)
        .then(body)
        .map(|((function_name, params), body)| {
            let params: Vec<String> = params
                .into_iter()
                .flatten()
                .map(|param| {
                    let Expr::Ident(param_name) = param else {
                        unreachable!();
                    };
                    param_name.clone()
                })
                .collect();

            let body = body.into_iter().flatten().collect();

            let Expr::Ident(name) = function_name else {
                unreachable!();
            };

            Stmt::FunctionDeclaration { name, params, body }
        })
}

pub fn parser<'a>() -> parser!(Token<'a> => Stmt<'a>) {
    recursive(|stmt| choice((function(stmt), var_declaration(), var_mutation()))).then_ignore(end())
}
