use logos::Logos;

#[derive(Logos, Clone, PartialEq, Hash, Debug, Eq)]
#[logos(error = String)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"//[^\n]*")]
pub enum Token {
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
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use super::*;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn lex(input: &str, expected: &[(Result<Token, String>, Range<usize>)]) {
        let a: Vec<(Result<Token, String>, Range<usize>)> = Token::lexer(input).spanned().collect();
        let b = expected.to_vec();

        assert_eq!(a, b);
    }

    #[test]
    fn addition() {
        lex(
            "1 + 3",
            &[
                (Ok(Token::Int(1)), 0..1),
                (Ok(Token::Plus), 2..3),
                (Ok(Token::Int(3)), 4..5),
            ],
        );
    }

    #[test]
    fn comments() {
        lex(
            "1 + 3 // hello world",
            &[
                (Ok(Token::Int(1)), 0..1),
                (Ok(Token::Plus), 2..3),
                (Ok(Token::Int(3)), 4..5),
            ],
        );
        lex(
            "1 + // hello world
            3",
            &[
                (Ok(Token::Int(1)), 0..1),
                (Ok(Token::Plus), 2..3),
                (Ok(Token::Int(3)), 31..32),
            ],
        );
    }
}
