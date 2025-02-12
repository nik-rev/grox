use chumsky::Parser as _;
use logos::Logos as _;

mod lexer;
mod parser;

fn main() {
    let text = "1 + 3 * (12 - 4)";

    let lexer = lexer::Token::lexer(text);

    let mut tokens = vec![];

    for (token, span) in lexer.spanned() {
        match token {
            Ok(token) => tokens.push(token),
            Err(e) => {
                eprintln!("lexer error at {:?}: {}", span, e);
                return;
            }
        }
    }

    println!("{:#?}", parser::parser().parse(tokens));
}
