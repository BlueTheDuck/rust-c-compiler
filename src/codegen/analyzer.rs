use super::context::{FunctionCtx, GlobalCtx};
use crate::{
    ast::{Expr, FuncDef, Stmt, VarDecl, VarDef},
    codegen::{context::Register, opcodes::*},
    tokens::expr::StackExpr,
};
use itertools::Itertools;

pub fn analyze_global(program: Vec<Stmt>) -> GlobalCtx {
    let mut ctx = GlobalCtx::default();
    for stmt in program {
        match stmt {
            Stmt::VarDecl(var) => {
                let addr = ctx.reserve_global_var(&var.ident);
                println!("{ident} is at {addr}", ident = var.ident, addr = addr);
            }
            Stmt::FuncDef(FuncDef {
                ty,
                ident,
                args,
                body,
            }) => {
                ctx.in_function(&ident, |ctx| {
                    for arg in args {
                        ctx.reserve_local(&arg.ident);
                    }
                    analyze_body(body, ctx);
                });
            }
            _ => panic!(
                "Global scope can only contain variable declarations and function definitions"
            ),
        }
    }
    return ctx;
}
pub fn analyze_body(body: Vec<Stmt>, ctx: &mut FunctionCtx) {
    for stmt in body {
        match stmt {
            Stmt::VarDecl(VarDecl { ty, ident }) => {
                ctx.reserve_local(&ident);
            }
            Stmt::VarDef(VarDef { ty, ident, expr }) => {
                let reg = ctx.reserve_local(&ident);
                compile_expression(ident, expr, ctx, reg);
            }
            Stmt::Assignment { lhs, rhs } => {
                // The assignment may use a variable both as a lhs and rhs
                // e.g.: x = x + 1
                // in that case we need to copy the value of x to a temporary register
                // otherwise we can just use the register of x as accumulator
                //
                // If the LHS is a global variable, we need to copy the value of the global variable to a temporary register
                let self_assign = rhs.list_idents().contains(&&lhs);
                if self_assign || ctx.address(&lhs).is_some() {
                    // If we are assigning to a variable that is also used as a rhs,
                    // or a global variable we reserve a temporary register
                    ctx.with_tmp(|ctx, acc| {
                        compile_expression(lhs, rhs, ctx, acc);
                    });
                } else if ctx.register(&lhs).is_some() {
                    // If we are NOT assigning to a variable that is also used as a rhs
                    // we can use it as the accumulator
                    let acc = ctx.register(&lhs).unwrap();
                    compile_expression(lhs, rhs, ctx, acc);
                } else {
                    panic!("'{lhs}' is neither a local variable nor a global variable");
                }
            }
            Stmt::Goto(label) => {
                ctx.push_asm_line(&format!("JMP {}", label));
            }
            Stmt::LabelDef(label) => {
                ctx.push_asm_line(&format!("{}:", label));
            }
            Stmt::IfNotZero { ident, body } => {
                ctx.push_asm_line(&format!("; if {ident} != 0"));
                let reg = ctx
                    .register(&ident)
                    .expect(&format!("'{ident}' is not a local variable"));
                ctx.with_tmp(|ctx, tmp| {
                    ctx.push_asm_line(&format!("SET {tmp}, 0"));
                    ctx.push_asm_line(&format!("CMP {reg}, {tmp}"));
                });
                let label_skip = ctx.gen_local_label("jz_skip");
                ctx.push_asm_line(&format!("JZ {label_skip}"));
                analyze_body(body, ctx);
                ctx.push_asm_line(&format!("{label_skip}:"));
            }
            Stmt::FuncDef(_) => panic!("Function definitions can't be nested"),
            #[allow(unreachable_patterns)]
            _ => todo!("Stmt {stmt:#?}"),
        }
    }
}

fn compile_expression(lhs: String, rhs: Expr, ctx: &mut FunctionCtx, acc: Register) {
    ctx.push_asm_line(&format!(
        "; compile_expression({lhs} := {rhs})",
        lhs = lhs,
        rhs = rhs
    ));
    ctx.with_tmp(|ctx, tmp| {
        let mut iter = rhs.stack.iter();
        let first = iter.next().unwrap();
        match first {
            StackExpr::Number(num) => ctx.push_asm_line(&format!("SET {acc}, {num:X}")),
            StackExpr::Ident(ident) => {
                if let Some(addr) = ctx.address(&ident) {
                    ctx.push_asm_line(format!("; copy global '{ident}' to accumulator"));
                    ctx.push_asm_line(
                        &Op::set_register(acc, addr.into())
                            .unwrap()
                            .unwrap()
                            .to_string(),
                    );
                } else if let Some(reg) = ctx.register(&ident) {
                    if let Some(op) = Op::set_register(acc, reg.into()).unwrap() {
                        ctx.push_asm_line(format!("; copy {ident} to accumulator"));
                        ctx.push_asm_line(&op.to_string());
                    }
                } else {
                    panic!("Undefined {ident}");
                }
            }
            _ => panic!("Unexpected expression {:#?}", first),
        }

        // The stack always follows the pattern: value (value op)*, so for the rest of the iterator
        // we can expect a value followed by an operator
        for (value, op) in iter
            .chunks(2)
            .into_iter()
            .map(|pair| pair.collect_tuple().unwrap())
        {
            match value {
                StackExpr::Number(num) => ctx.push_asm_line(&format!("SET {tmp}, {num:X}")),
                StackExpr::Ident(ident) => {
                    let operand: Operand = match (ctx.register(ident), ctx.address(ident)) {
                        (Some(reg), None) => reg.into(),
                        (None, Some(addr)) => addr.into(),
                        (None, None) => panic!("Undefined {ident}"),
                        (Some(_), Some(_)) => panic!("Variable {ident} is both local and global"),
                    };
                    if let Some(op) = Op::set_register(tmp, operand).unwrap() {
                        ctx.push_asm_line(&op.to_string());
                    }
                }
                _ => panic!("Expected number/ident. Got {:#?}", value),
            }
            match op {
                StackExpr::Add => {
                    ctx.push_asm_line(&format!("ADD {acc}, {tmp}"));
                }
                StackExpr::Sub => {
                    ctx.push_asm_line(&format!("SUB {acc}, {tmp}"));
                }
                StackExpr::Shl => todo!(),
                StackExpr::Shr => todo!(),
                _ => panic!("Unexpected operator. Got {:#?}", op),
            }
        }

        let operand: Operand = match (ctx.register(&lhs), ctx.address(&lhs)) {
            (Some(reg), None) => reg.into(),
            (None, Some(addr)) => addr.into(),
            _ => unreachable!(),
        };
        if let Some(op) = Op::move_value(operand, acc.into()).unwrap() {
            ctx.push_asm_line(&op.to_string());
        }
    });
}
