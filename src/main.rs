use std::{collections::HashMap, iter};

use chumsky::Parser as _;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue,
        InstructionValue, PointerValue,
    },
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

    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    fn build_load(&self, ptr: PointerValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        self.builder
            .build_load(self.context.f64_type(), ptr, name)
            .unwrap()
    }

    /// Allocate a new pointer to an f64 on the stack
    fn create_ptr(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name).unwrap()
    }

    fn alloc<V: BasicValue<'ctx>>(&self, ptr: PointerValue<'ctx>, val: V) -> InstructionValue {
        self.builder.build_store(ptr, val).unwrap()
    }

    fn compile_fn_decl(&mut self, func: Stmt) -> Result<FunctionValue<'ctx>, &'static str> {
        let Stmt::FunctionDeclaration { name, params, body } = func else {
            panic!("Not a function")
        };

        let return_type = self.context.f64_type();
        let args_types: Vec<BasicMetadataTypeEnum> = iter::repeat(return_type)
            .take(params.len())
            .map(|f| f.into())
            .collect();

        let fn_type = self.context.f64_type().fn_type(&args_types, false);
        let fn_val = self.module.add_function(&name, fn_type, None);

        // set argument names
        for (param, param_name) in fn_val.get_param_iter().zip(params.iter()) {
            param.into_float_value().set_name(param_name);
        }

        let entry = self.context.append_basic_block(fn_val, "entry");

        self.builder.position_at_end(entry);

        let len = params.len();

        self.variables.reserve(len);

        for (param_val, param_name) in fn_val.get_param_iter().zip(params) {
            let param_ptr = self.create_ptr(&param_name);

            self.alloc(param_ptr, param_val);

            self.variables.insert(param_name, param_ptr);
        }

        let Some(body) = self.compile_stmts(body) else {
            // TODO: handle functions with no return
            unreachable!()
        };

        self.builder.build_return(Some(&body?)).unwrap();

        if fn_val.verify(true) {
            Ok(fn_val)
        } else {
            // SAFETY: fn_val is not used after being deleted as it is dropped
            // and not returned from the function
            unsafe {
                fn_val.delete();
            }

            Err("Invalid generated function")
        }
    }

    /// Returns Some(Result<FloatValue>) if the statement is a return. Stops
    /// processing all statements in that case.
    fn compile_stmts(
        &mut self,
        stmts: Vec<Stmt>,
    ) -> Option<Result<FloatValue<'ctx>, &'static str>> {
        for stmt in stmts {
            match stmt {
                Stmt::FunctionDeclaration { name, params, body } => {
                    if let Err(err) =
                        self.compile_fn_decl(Stmt::FunctionDeclaration { name, params, body })
                    {
                        return Some(Err(err));
                    };
                }
                // With 'let'
                Stmt::VariableDeclaration { name, value } => {
                    let var_value = match *value {
                        Some(value) => match self.compile_expr(value) {
                            Ok(ok) => ok,
                            Err(err) => return Some(Err(err)),
                        },
                        // empty variables initialize to zero
                        None => self.context.f64_type().const_float(0.),
                    };

                    let var_ptr = self.create_ptr(name.as_str());

                    self.alloc(var_ptr, var_value);
                    self.variables.insert(name, var_ptr);
                }
                // In Grox we allow shadowing variables.
                Stmt::VariableAssignment { name, value } => {
                    let var_value = match self.compile_expr(*value) {
                        Ok(ok) => ok,
                        Err(err) => return Some(Err(err)),
                    };

                    let var_ptr = match self.variables.get(name).ok_or("Undefined variable.") {
                        Ok(ok) => ok,
                        Err(err) => return Some(Err(err)),
                    };

                    self.alloc(*var_ptr, var_value);
                }
                Stmt::Return(expr) => return Some(self.compile_expr(*expr)),
            }
        }

        // Statements have been successfully compiled, but no 'return' statement found
        None
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
