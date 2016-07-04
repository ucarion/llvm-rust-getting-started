#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate llvm_sys as llvm;

use std::fs::File;
use std::ptr;
use std::io::Read;
use std::ffi::CString;
use std::collections::{HashMap, HashSet};

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

pub enum Expr {
    Literal(String),
    Ref(String),
    Assign(String, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Vec<Expr>, Vec<Expr>),
}

// `product` and `sum` are that way to get operator precedence
peg! parser(r#"
    use super::Expr;

    #[pub]
    program -> Vec<Expr>
        = statements

    statements -> Vec<Expr>
        = e:(expression ** ("\n" _)) "\n" { e }

    expression -> Expr
        = if_expression
        / i:identifier _ "=" _ s:expression { Expr::Assign(i, Box::new(s)) }
        / sum

    if_expression -> Expr
        = "if" _ e:expression _ "{\n" _ then_body:statements _ "}" _ "else" _ "{\n" _ else_body:statements _ "}" {
            Expr::If(Box::new(e), then_body, else_body)
        }

    sum -> Expr
        = a:product _ "+" _ b:sum { Expr::Add(Box::new(a), Box::new(b)) }
        / a:product _ "-" _ b:sum { Expr::Sub(Box::new(a), Box::new(b)) }
        / product

    product -> Expr
        = a:ref_or_literal _ "*" _ b:product { Expr::Mul(Box::new(a), Box::new(b)) }
        / a:ref_or_literal _ "/" _ b:product { Expr::Div(Box::new(a), Box::new(b)) }
        / ref_or_literal

    ref_or_literal -> Expr
        = i:identifier { Expr::Ref(i) }
        / int_literal

    identifier -> String
        = [a-zA-Z]+ { match_str.to_owned() }

    int_literal -> Expr
        = [0-9]+ { Expr::Literal(match_str.to_owned()) }

    _ = [ \t]*
"#);

unsafe fn codegen(input: Vec<Expr>) {
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

    let mut names = HashMap::new();
    insert_allocations(context, builder, &mut names, &input);

    let int_type = llvm::core::LLVMInt64TypeInContext(context);
    let zero = llvm::core::LLVMConstInt(int_type, 0, 0);

    let mut return_value = zero; // return value on empty program
    for expr in input {
        return_value = codegen_expr(context, builder, function, &mut names, expr);
    }
    llvm::core::LLVMBuildRet(builder, return_value);

    // Instead of dumping to stdout, let's write out the IR to `out.ll`
    let out_file = CString::new("out.ll").unwrap();
    llvm::core::LLVMPrintModuleToFile(module, out_file.as_ptr(), ptr::null_mut());

    llvm::core::LLVMDisposeBuilder(builder);
    llvm::core::LLVMDisposeModule(module);
    llvm::core::LLVMContextDispose(context);
}

unsafe fn insert_allocations(context: LLVMContextRef, builder: LLVMBuilderRef, names: &mut HashMap<String, LLVMValueRef>, exprs: &[Expr]) {
    let mut variable_names = HashSet::new();
    for expr in exprs {
        match *expr {
            Expr::Assign(ref name, _) => {
                variable_names.insert(name);
            },

            _ => {},
        }
    }

    for variable_name in variable_names {
        let int_type = llvm::core::LLVMInt64TypeInContext(context);
        let name = CString::new(variable_name.as_bytes()).unwrap();
        let pointer = llvm::core::LLVMBuildAlloca(builder, int_type, name.as_ptr());

        names.insert(variable_name.to_owned(), pointer);
    }
}

// When you write out instructions in LLVM, you get back `LLVMValueRef`s. You
// can then use these references in other instructions.
unsafe fn codegen_expr(context: LLVMContextRef, builder: LLVMBuilderRef, func: LLVMValueRef, names: &mut HashMap<String, LLVMValueRef>, expr: Expr) -> LLVMValueRef {
    match expr {
        Expr::Literal(int_literal) => {
            let int_type = llvm::core::LLVMInt64TypeInContext(context);
            llvm::core::LLVMConstInt(int_type, int_literal.parse().unwrap(), 0)
        },

        Expr::Add(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, func, names, *lhs);
            let rhs = codegen_expr(context, builder, func, names, *rhs);

            let name = CString::new("addtmp").unwrap();
            llvm::core::LLVMBuildAdd(builder, lhs, rhs, name.as_ptr())
        },

        Expr::Sub(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, func, names, *lhs);
            let rhs = codegen_expr(context, builder, func, names, *rhs);

            let name = CString::new("subtmp").unwrap();
            llvm::core::LLVMBuildSub(builder, lhs, rhs, name.as_ptr())
        },

        Expr::Mul(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, func, names, *lhs);
            let rhs = codegen_expr(context, builder, func, names, *rhs);

            let name = CString::new("multmp").unwrap();
            llvm::core::LLVMBuildMul(builder, lhs, rhs, name.as_ptr())
        },

        Expr::Div(lhs, rhs) => {
            let lhs = codegen_expr(context, builder, func, names, *lhs);
            let rhs = codegen_expr(context, builder, func, names, *rhs);

            let name = CString::new("divtmp").unwrap();
            llvm::core::LLVMBuildUDiv(builder, lhs, rhs, name.as_ptr())
        },

        Expr::Ref(name) => {
            let pointer = names.get(&name).unwrap();
            let name = CString::new(name).unwrap();
            llvm::core::LLVMBuildLoad(builder, *pointer, name.as_ptr())
        },

        Expr::Assign(name, expr) => {
            let new_value = codegen_expr(context, builder, func, names, *expr);
            let pointer = names.get(&name).unwrap();
            llvm::core::LLVMBuildStore(builder, new_value, *pointer);
            new_value
        },

        Expr::If(condition, then_body, else_body) => {
            let condition_value = codegen_expr(context, builder, func, names, *condition);
            let int_type = llvm::core::LLVMInt64TypeInContext(context);
            let zero = llvm::core::LLVMConstInt(int_type, 0, 0);

            let name = CString::new("is_nonzero").unwrap();
            let is_nonzero = llvm::core::LLVMBuildICmp(builder, llvm::LLVMIntPredicate::LLVMIntNE, condition_value, zero, name.as_ptr());

            let entry_name = CString::new("entry").unwrap();
            let then_block = llvm::core::LLVMAppendBasicBlockInContext(context, func, entry_name.as_ptr());
            let else_block = llvm::core::LLVMAppendBasicBlockInContext(context, func, entry_name.as_ptr());
            let merge_block = llvm::core::LLVMAppendBasicBlockInContext(context, func, entry_name.as_ptr());

            llvm::core::LLVMBuildCondBr(builder, is_nonzero, then_block, else_block);

            llvm::core::LLVMPositionBuilderAtEnd(builder, then_block);
            let mut then_return = zero;
            for expr in then_body {
                then_return = codegen_expr(context, builder, func, names, expr);
            }
            llvm::core::LLVMBuildBr(builder, merge_block);
            let then_block = llvm::core::LLVMGetInsertBlock(builder);

            llvm::core::LLVMPositionBuilderAtEnd(builder, else_block);
            let mut else_return = zero;
            for expr in else_body {
                else_return = codegen_expr(context, builder, func, names, expr);
            }
            llvm::core::LLVMBuildBr(builder, merge_block);
            let else_block = llvm::core::LLVMGetInsertBlock(builder);

            llvm::core::LLVMPositionBuilderAtEnd(builder, merge_block);
            let phi_name = CString::new("iftmp").unwrap();
            let phi = llvm::core::LLVMBuildPhi(builder, int_type, phi_name.as_ptr());

            let mut values = vec![then_return, else_return];
            let mut blocks = vec![then_block, else_block];

            llvm::core::LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
            phi
        },
    }
}
