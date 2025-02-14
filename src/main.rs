use std::collections::HashMap;

use chumsky::Parser as _;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue, PointerValue},
};
use logos::Logos as _;
use parser::{Expr, Stmt};

mod lexer;
mod parser;

struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    fn build_load(&self, ptr: PointerValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        self.builder
            .build_load(self.context.f64_type(), ptr, name)
            .unwrap()
    }

    fn compile_expr(&mut self, expr: Expr) -> Result<FloatValue<'ctx>, &'static str> {
        match expr {
            Expr::Number(num) => Ok(self.context.f64_type().const_float(*num)),
            Expr::Ident(name) => match self.variables.get(name.as_str()) {
                Some(var) => Ok(self.build_load(*var, name.as_str()).into_float_value()),
                None => Err("No variable found"),
            },
            Expr::Neg(expr) => {
                self.compile_expr(Expr::Mul(expr.clone(), Box::new(Expr::Number(2.0.into()))))
            }
            Expr::Add(lhs, rhs) => {
                let lhs = self.compile_expr(*lhs)?;
                let rhs = self.compile_expr(*rhs)?;
                Ok(self.builder.build_float_add(lhs, rhs, "tmpadd").unwrap())
            }
            Expr::Sub(lhs, rhs) => {
                let lhs = self.compile_expr(*lhs)?;
                let rhs = self.compile_expr(*rhs)?;
                Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub").unwrap())
            }
            Expr::Mul(lhs, rhs) => {
                let lhs = self.compile_expr(*lhs)?;
                let rhs = self.compile_expr(*rhs)?;
                Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul").unwrap())
            }
            Expr::Div(lhs, rhs) => {
                let lhs = self.compile_expr(*lhs)?;
                let rhs = self.compile_expr(*rhs)?;
                Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv").unwrap())
            }
            Expr::Call(name, args) => {
                let Some(fun) = self.get_function(&name) else {
                    return Err("Unknown function");
                };

                let mut compiled_args = Vec::with_capacity(args.len());

                for arg in args {
                    compiled_args.push(self.compile_expr(arg)?);
                }

                let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                    .iter()
                    .by_ref()
                    .map(|&val| val.into())
                    .collect();

                let Some(val) = self
                    .builder
                    .build_call(fun, &argsv, "tmp")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                else {
                    return Err("Invalid call produced");
                };

                Ok(val.into_float_value())
            }
        }
    }
}

// impl<'ctx> CodeGen<'ctx> {
//     fn
// }

fn main() {
    let text = "\
    fn main(b, a) {
        let lol = 4 + xd(a, 4);
    }";

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

    let ast = parser::parser().parse(tokens);

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("grox");
}
