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
pub enum Stmt<'a> {
    FunctionDeclaration {
        name: String,
        params: Vec<Vec<Token<'a>>>,
        body: Vec<Vec<Stmt<'a>>>,
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

pub fn parser2<'a>() -> impl Parser<Token<'a>, Stmt<'a>, Error = Simple<Token<'a>>> {
    recursive(|stmt| {
        let func = just(Token::Ident("fn"))
            .ignore_then(just(Token::Ident("main")))
            // parameters
            .then(
                just(Token::Ident("lol"))
                    .repeated()
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )
            // body
            .then(
                stmt.repeated()
                    .separated_by(just(Token::Semicolon))
                    .delimited_by(just(Token::OpenCurly), just(Token::CloseCurly)),
            )
            .map(|((name, params), body)| {
                let Token::Ident(name) = name else {
                    unreachable!();
                };

                Stmt::FunctionDeclaration {
                    name: name.to_owned(),
                    params,
                    body,
                }
            });

        func

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

pub fn parser<'a>() -> impl Parser<Token<'a>, Expr, Error = Simple<Token<'a>>> {
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
