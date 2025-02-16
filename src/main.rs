use std::env::{self, current_dir};

use chumsky::Parser as _;
use inkwell::{
    context::Context,
    llvm_sys::target::{
        LLVM_InitializeAllAsmParsers, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos,
        LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets, LLVM_InitializeNativeTarget,
    },
    object_file::ObjectFile,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine, TargetTriple},
    OptimizationLevel,
};
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

    let mut compiler = codegen::Compiler::new(&context, builder, module.clone());

    let fn_main = compiler.compile(ast);

    fn_main.set_linkage(inkwell::module::Linkage::External);

    unsafe {
        LLVM_InitializeNativeTarget();
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllAsmPrinters();
    }

    let target_triple = TargetMachine::get_default_triple();
    dbg!(&target_triple);
    let target = Target::from_triple(&target_triple).unwrap();

    let machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::None,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    module.set_triple(&target_triple);
    module.set_data_layout(&machine.get_target_data().get_data_layout());

    module.print_to_stderr();

    dbg!(&module, &fn_main);

    let path = env::current_dir().unwrap().join("grox.o");

    machine
        .write_to_file(&module, FileType::Object, &path)
        .unwrap();
}
