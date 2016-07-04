#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate llvm_sys as llvm;

use std::fs::File;
use std::ptr;
use std::io::Read;
use std::ffi::CString;

use llvm::prelude::*;

fn main() {
    let mut input = String::new();
    let mut f = File::open("in.ex").unwrap();
    f.read_to_string(&mut input).unwrap();

    let parsed_input = parser::program(&input).unwrap();

    unsafe {
        codegen(parsed_input);
    }
}

// Some sort of "expression", in our case it's just arithmetic
pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Literal(String),
}

// `product` and `sum` are that way to get operator precedence
peg! parser(r#"
    use super::Expr;

    #[pub]
    program -> Expr
        = e:expression "\n" { e }

    expression -> Expr
        = sum

    sum -> Expr
        = a:product _ "+" _ b:sum { Expr::Add(Box::new(a), Box::new(b)) }
        / a:product _ "-" _ b:sum { Expr::Sub(Box::new(a), Box::new(b)) }
        / product

    product -> Expr
        = a:int_literal _ "*" _ b:product { Expr::Mul(Box::new(a), Box::new(b)) }
        / a:int_literal _ "/" _ b:product { Expr::Div(Box::new(a), Box::new(b)) }
        / int_literal

    int_literal -> Expr
        = [0-9]+ { Expr::Literal(match_str.to_owned()) }

    _ = " "*
"#);

unsafe fn codegen(input: Expr) {
    let context = llvm::core::LLVMContextCreate();
    let module = llvm::core::LLVMModuleCreateWithName(b"example_module\0".as_ptr() as *const _);
    let builder = llvm::core::LLVMCreateBuilderInContext(context);

    // In LLVM, you get your types from functions.
    let int_type = llvm::core::LLVMInt64TypeInContext(context);
    let function_type = llvm::core::LLVMFunctionType(int_type, ptr::null_mut(), 0, 0);
    let function = llvm::core::LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);

    let entry_name = CString::new("entry").unwrap();
    let bb = llvm::core::LLVMAppendBasicBlockInContext(context, function, entry_name.as_ptr());
    llvm::core::LLVMPositionBuilderAtEnd(builder, bb);

    // The juicy part: construct a `LLVMValue` from a Rust value:
    let return_value = codegen_expr(context, builder, input);

    llvm::core::LLVMBuildRet(builder, return_value);

    // Instead of dumping to stdout, let's write out the IR to `out.ll`
    let out_file = CString::new("out.ll").unwrap();
    llvm::core::LLVMPrintModuleToFile(module, out_file.as_ptr(), ptr::null_mut());

    llvm::core::LLVMDisposeBuilder(builder);
    llvm::core::LLVMDisposeModule(module);
    llvm::core::LLVMContextDispose(context);
}

// When you write out instructions in LLVM, you get back `LLVMValueRef`s. You
// can then use these references in other instructions.
unsafe fn codegen_expr(context: LLVMContextRef, builder: LLVMBuilderRef, expr: Expr) -> LLVMValueRef {
    match expr {
        Expr::Literal(int_literal) => {
            let int_type = llvm::core::LLVMInt64TypeInContext(context);
            llvm::core::LLVMConstInt(int_type, int_literal.parse().unwrap(), 0)
        },

        Expr::Add(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, *lhs);
            let rhs = codegen_expr(context, builder, *rhs);

            let name = CString::new("addtmp").unwrap();
            llvm::core::LLVMBuildAdd(builder, lhs, rhs, name.as_ptr())
        },

        Expr::Sub(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, *lhs);
            let rhs = codegen_expr(context, builder, *rhs);

            let name = CString::new("subtmp").unwrap();
            llvm::core::LLVMBuildSub(builder, lhs, rhs, name.as_ptr())
        },

        Expr::Mul(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, *lhs);
            let rhs = codegen_expr(context, builder, *rhs);

            let name = CString::new("multmp").unwrap();
            llvm::core::LLVMBuildMul(builder, lhs, rhs, name.as_ptr())
        },

        Expr::Div(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, *lhs);
            let rhs = codegen_expr(context, builder, *rhs);

            let name = CString::new("divtmp").unwrap();
            llvm::core::LLVMBuildUDiv(builder, lhs, rhs, name.as_ptr())
        },
    }
}
