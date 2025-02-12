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

    #[test]
    fn function() {
        lex(
            "fn main() {}",
            &[
                (Ok(T::Ident("fn")), 0..2),
                (Ok(T::Ident("main")), 3..7),
                (Ok(T::OpenParen), 7..8),
                (Ok(T::CloseParen), 8..9),
                (Ok(T::OpenCurly), 10..11),
                (Ok(T::CloseCurly), 11..12),
            ],
        );
        lex(
            "\
fn main() {
    let a = 4;
    let x = a * 4;
    x - a
}",
            &[
                (Ok(T::Ident("fn")), 0..2),
                (Ok(T::Ident("main")), 3..7),
                (Ok(T::OpenParen), 7..8),
                (Ok(T::CloseParen), 8..9),
                (Ok(T::OpenCurly), 10..11),
                (Ok(T::Ident("let")), 16..19),
                (Ok(T::Ident("a")), 20..21),
                (Ok(T::Equals), 22..23),
                (Ok(T::Int(4)), 24..25),
                (Ok(T::Semicolon), 25..26),
                (Ok(T::Ident("let")), 31..34),
                (Ok(T::Ident("x")), 35..36),
                (Ok(T::Equals), 37..38),
                (Ok(T::Ident("a")), 39..40),
                (Ok(T::Multiply), 41..42),
                (Ok(T::Int(4)), 43..44),
                (Ok(T::Semicolon), 44..45),
                (Ok(T::Ident("x")), 50..51),
                (Ok(T::Minus), 52..53),
                (Ok(T::Ident("a")), 54..55),
                (Ok(T::CloseCurly), 56..57),
            ],
        );
    }
}
