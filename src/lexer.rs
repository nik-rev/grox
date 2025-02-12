use logos::Logos;

#[derive(Logos, Clone, PartialEq, Hash, Debug, Eq)]
#[logos(error = String)]
// whitespace
#[logos(skip r"[ \t\r\n\f]+")]
// comments
#[logos(skip r"//[^\n]*")]
pub enum Token<'a> {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("{")]
    OpenCurly,
    #[token("}")]
    CloseCurly,
    #[token(",")]
    Comma,
    #[token("=")]
    Equals,
    #[token(";")]
    Semicolon,
    #[regex("[0-9]+", |lex| lex.slice().parse::<isize>().unwrap())]
    Int(isize),
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'a str),
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use super::Token as T;
    use super::*;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn lex_valid(source: &str, expected: &[Token]) {
        let actual: Vec<Token> = T::lexer(source).map(|a| a.unwrap()).collect();
        let expected = expected.to_vec();

        assert_eq!(expected, actual);
    }

    #[test]
    fn addition() {
        lex_valid("1 + 3", &[T::Int(1), T::Plus, T::Int(3)]);
    }

    #[test]
    fn comments() {
        lex_valid("1 + 3 // hello world", &[T::Int(1), T::Plus, T::Int(3)]);
        lex_valid(
            "1 + // hello world
            3",
            &[T::Int(1), T::Plus, T::Int(3)],
        );
    }

    #[test]
    fn identifier() {
        lex_valid(
            "1 + hello * 4",
            &[
                T::Int(1),
                T::Plus,
                T::Ident("hello"),
                T::Multiply,
                T::Int(4),
            ],
        )
    }

    #[test]
    fn function() {
        lex_valid(
            "fn main() {}",
            &[
                T::Ident("fn"),
                T::Ident("main"),
                T::OpenParen,
                T::CloseParen,
                T::OpenCurly,
                T::CloseCurly,
            ],
        );
        lex_valid(
            "\
fn main(z, m) {
    let a = 4;
    let x = a * 4;
    x - z * m
}",
            &[
                T::Ident("fn"),
                T::Ident("main"),
                T::OpenParen,
                T::Ident("z"),
                T::Comma,
                T::Ident("m"),
                T::CloseParen,
                T::OpenCurly,
                T::Ident("let"),
                T::Ident("a"),
                T::Equals,
                T::Int(4),
                T::Semicolon,
                T::Ident("let"),
                T::Ident("x"),
                T::Equals,
                T::Ident("a"),
                T::Multiply,
                T::Int(4),
                T::Semicolon,
                T::Ident("x"),
                T::Minus,
                T::Ident("z"),
                T::Multiply,
                T::Ident("m"),
                T::CloseCurly,
            ],
        );
    }
}
