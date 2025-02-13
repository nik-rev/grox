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
}

#[derive(Debug)]
pub enum Stmt {
    FunctionDeclaration {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    VariableDeclaration {
        name: String,
        value: Box<Expr>,
    },
    VariableAssignment {
        name: String,
        value: Box<Expr>,
    },
    Return(Box<Expr>),
}

// TODO: Replace this macro with a type alias once that feature lands on stable Rust
macro_rules! parser {
    ($from:ty => $to:tt) => {
        impl chumsky::Parser<$from, $to, Error = chumsky::error::Simple<$from>>
    };
}

fn ident<'a>() -> parser!(Token<'a> => Expr) {
    select!(|_span| Token::Ident(ident) => Expr::Ident(ident.to_owned()))
}

fn function<'a>(stmt: parser!(Token<'a> => Stmt)) -> parser!(Token<'a> => Stmt) {
    let name = ident();

    let parameters = ident()
        .repeated()
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

    let body = stmt
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

pub fn parser2<'a>() -> parser!(Token<'a> => Stmt) {
    recursive(|stmt| {
        function(stmt)

        // let atom = {
        //     let parenthesized = expr
        //         .clone()
        //         .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

        //     let integer = select!(|_span| Token::Int(n) => Expr::Int(n));

        //     parenthesized.or(integer)
        // };

        // let unary = just(Token::Minus)
        //     .repeated()
        //     .then(atom)
        //     .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        // let product = unary
        //     .clone()
        //     .then(
        //         just(Token::Multiply)
        //             .or(just(Token::Divide))
        //             .then(unary)
        //             .repeated(),
        //     )
        //     .foldl(|lhs, (op, rhs)| match op {
        //         Token::Multiply => Expr::Mul(Box::new(lhs), Box::new(rhs)),
        //         Token::Divide => Expr::Div(Box::new(lhs), Box::new(rhs)),
        //         _ => unreachable!(),
        //     });

        // let sum = product
        //     .clone()
        //     .then(
        //         just(Token::Plus)
        //             .or(just(Token::Minus))
        //             .then(product)
        //             .repeated(),
        //     )
        //     .foldl(|lhs, (op, rhs)| match op {
        //         Token::Plus => Expr::Add(Box::new(lhs), Box::new(rhs)),
        //         Token::Minus => Expr::Sub(Box::new(lhs), Box::new(rhs)),
        //         _ => unreachable!(),
        //     });

        // sum
    })
    .then_ignore(end())
}

pub fn parser<'a>() -> parser!(Token<'a> => Expr) {
    recursive(|expr| {
        let atom = {
            let parenthesized = expr
                .clone()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

            let integer = select!(|_span| Token::Int(n) => Expr::Int(n));

            parenthesized.or(integer)
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
    .then_ignore(end())
}
