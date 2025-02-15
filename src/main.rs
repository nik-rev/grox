use chumsky::Parser as _;
use inkwell::context::Context;
use logos::Logos as _;

mod codegen;
mod lexer;
mod parser;

fn main() {
    let text = "\
fn lmao(x) {
    return 4 - x;
}
    
fn main() {
    let lol = 4 + 16;
    return 1 + lol * lmao(8);
}";

    let mut tokens = vec![];

    for (token, span) in lexer::Token::lexer(text).spanned() {
        match token {
            Ok(token) => tokens.push(token),
            Err(e) => {
                eprintln!("lexer error at {:?}: {}", span, e);
                return;
            }
        }
    }

    let ast = parser::parser().parse(tokens).unwrap();

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("grox");

    let mut compiler = codegen::Compiler::new(&context, builder, module);

    compiler.compile(ast);
}
