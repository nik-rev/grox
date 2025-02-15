use logos::Logos;
use ordered_float::NotNaN;

#[derive(Logos, Clone, PartialEq, Debug, Hash, Eq, PartialOrd, Ord)]
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
    #[regex("([0-9]*[.])?[0-9]+", |lex| NotNaN::<f64>::new(lex.slice().parse::<f64>().unwrap()).unwrap())]
    Float(NotNaN<f64>),
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'a str),
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn lex_valid(source: &str, expected: &[Token]) {
        let actual: Vec<Token> = Token::lexer(source).map(|a| a.unwrap()).collect();
        let expected = expected.to_vec();

        assert_eq!(expected, actual);
    }

    #[test]
    fn addition() {
        lex_valid("1 + 3", &[Float(1.0.into()), Plus, Float(3.0.into())]);
    }

    #[test]
    fn comments() {
        lex_valid(
            "1 + 3 // hello world",
            &[Float(1.0.into()), Plus, Float(3.0.into())],
        );
        lex_valid(
            "1 + // hello world
            3",
            &[Float(1.0.into()), Plus, Float(3.0.into())],
        );
    }

    #[test]
    fn identifier() {
        lex_valid(
            "1 + hello * 4",
            &[
                Float(1.0.into()),
                Plus,
                Ident("hello"),
                Multiply,
                Float(4.0.into()),
            ],
        )
    }

    #[test]
    fn function() {
        lex_valid(
            "fn main() {}",
            &[
                Ident("fn"),
                Ident("main"),
                OpenParen,
                CloseParen,
                OpenCurly,
                CloseCurly,
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
                Ident("fn"),
                Ident("main"),
                OpenParen,
                Ident("z"),
                Comma,
                Ident("m"),
                CloseParen,
                OpenCurly,
                Ident("let"),
                Ident("a"),
                Equals,
                Float(4.0.into()),
                Semicolon,
                Ident("let"),
                Ident("x"),
                Equals,
                Ident("a"),
                Multiply,
                Float(4.0.into()),
                Semicolon,
                Ident("x"),
                Minus,
                Ident("z"),
                Multiply,
                Ident("m"),
                CloseCurly,
            ],
        );
    }
}
