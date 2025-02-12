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
    LeftParen,
    #[token(")")]
    RightParen,
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
    fn lex(input: &str, expected: &[(Result<Token, String>, Range<usize>)]) {
        let a: Vec<(Result<Token, String>, Range<usize>)> = T::lexer(input).spanned().collect();
        let b = expected.to_vec();

        assert_eq!(b, a);
    }

    #[test]
    fn addition() {
        lex(
            "1 + 3",
            &[
                (Ok(T::Int(1)), 0..1),
                (Ok(T::Plus), 2..3),
                (Ok(T::Int(3)), 4..5),
            ],
        );
    }

    #[test]
    fn comments() {
        lex(
            "1 + 3 // hello world",
            &[
                (Ok(T::Int(1)), 0..1),
                (Ok(T::Plus), 2..3),
                (Ok(T::Int(3)), 4..5),
            ],
        );
        lex(
            "1 + // hello world
            3",
            &[
                (Ok(T::Int(1)), 0..1),
                (Ok(T::Plus), 2..3),
                (Ok(T::Int(3)), 31..32),
            ],
        );
    }

    #[test]
    fn identifier() {
        lex(
            "1 + hello * 4",
            &[
                (Ok(T::Int(1)), 0..1),
                (Ok(T::Plus), 2..3),
                (Ok(T::Ident("hello")), 4..9),
                (Ok(T::Multiply), 10..11),
                (Ok(T::Int(4)), 12..13),
            ],
        )
    }
}
