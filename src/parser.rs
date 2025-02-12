use chumsky::prelude::*;

use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    // Integer literal
    Int(isize),

    // Unary minus
    Neg(Box<Expr>),

    // Binary operators
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

pub fn parser<'a>() -> impl Parser<Token<'a>, Expr, Error = Simple<Token<'a>>> {
    recursive(|p| {
        let atom = {
            let parenthesized = p
                .clone()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen));

            let integer = select!(|_span|Token::Int(n) => Expr::Int(n));

            parenthesized.or(integer)
        };

        let unary = just(Token::Minus)
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let binary_1 = unary
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

        binary_1
            .clone()
            .then(
                just(Token::Plus)
                    .or(just(Token::Minus))
                    .then(binary_1)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| match op {
                Token::Plus => Expr::Add(Box::new(lhs), Box::new(rhs)),
                Token::Minus => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            })
    })
    .then_ignore(end())
}
