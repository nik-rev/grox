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
        lex_valid("1 + 3", &[Int(1), Plus, Int(3)]);
    }

    #[test]
    fn comments() {
        lex_valid("1 + 3 // hello world", &[Int(1), Plus, Int(3)]);
        lex_valid(
            "1 + // hello world
            3",
            &[Int(1), Plus, Int(3)],
        );
    }

    #[test]
    fn identifier() {
        lex_valid(
            "1 + hello * 4",
            &[Int(1), Plus, Ident("hello"), Multiply, Int(4)],
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
                Int(4),
                Semicolon,
                Ident("let"),
                Ident("x"),
                Equals,
                Ident("a"),
                Multiply,
                Int(4),
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
