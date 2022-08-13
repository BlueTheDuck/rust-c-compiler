use crate::{
    ast::{Expr, Stmt, VarDef},
    tokens::expr::StackExpr,
};

use super::context::{CompilationCtx, Register};

pub fn analyze(program: Vec<Stmt>) -> CompilationCtx {
    let mut ctx = CompilationCtx::default();
    analyzer_scoped(program, &mut ctx);
    ctx
}

fn analyzer_scoped(program: Vec<Stmt>, ctx: &mut CompilationCtx) {
    for stmt in program {
        match stmt {
            Stmt::FuncDef(func) => {
                ctx.push_scope(&func.ident);
                for arg in func.args {
                    ctx.reserve_register(&arg.ident, None);
                }
                analyzer_scoped(func.body, ctx);
                ctx.pop_scope();
            }
            Stmt::VarDef(VarDef { ty, ident, expr }) => {
                assert!(
                    !ctx.is_global(),
                    "Global variables can't have initial values (yet)"
                );
                let reg = ctx.reserve_register(&ident, None);
                compile_expression(ident, expr, ctx, Some(reg));
            }
            Stmt::VarDecl(var) => {
                // variable declarations can be either global or local
                // for global variables we assigna a memory location
                // for local variables we assign a register
                if ctx.is_global() {
                    ctx.reserve_global_var(&var.ident);
                } else {
                    ctx.reserve_register(&var.ident, None);
                }
            }
            Stmt::Assignment { lhs, rhs } => {
                ctx.push_asm_line(&format!("{} = {}", lhs, rhs));
                // The assignment may use a variable both as a lhs and rhs
                // e.g.: x = x + 1
                // in that case we need to copy the value of x to a temporary register
                // but otherwise we can just use the register of x as the lhs
                let self_assign = !rhs.list_idents().contains(&&lhs);
                if self_assign {
                    // If we are assigning to a variable that is also used as a rhs
                    // we let `compile_expression` reserve a temporary register for us
                    compile_expression(lhs, rhs, ctx, None);
                } else {
                    let reg = ctx.get_register(&lhs);
                    compile_expression(lhs, rhs, ctx, reg);
                }
                
            },
            Stmt::LabelDef(ident) => {
                ctx.push_asm_line(&format!("{}:", ident));
            }
            Stmt::Goto(ident) => {
                ctx.push_asm_line(&format!("JMP {}", ident));
            }
            #[allow(unreachable_patterns)]
            _ => todo!("Stmt {stmt:#?}")
        }
    }
}

fn compile_expression(lhs: String, rhs: Expr, ctx: &mut CompilationCtx, acc_reg: Option<Register>) {
    let _acc = acc_reg.unwrap_or_else(|| ctx.reserve_register("_acc", None));
    let _tmp = ctx.reserve_register("_tmp", None);
    let mut iter = rhs.stack.iter();
    let first = iter.next().unwrap();
    match first {
        StackExpr::Number(num) => {
            ctx.push_asm_line(&format!("SET {}, {:X}", _acc, num));
        }
        StackExpr::Ident(ident) => {
            if ctx.var_is_global(ident) {
                let rhs_addr = ctx.get_var_addr(ident).unwrap();
                ctx.push_asm_line(&format!("LOAD {}, [{}]", _acc, rhs_addr));
            } else if ctx.var_is_local(ident) {
                let rhs_reg = ctx.get_register(ident).unwrap();
                ctx.push_asm_line(&format!("MOV {}, {}", _acc, rhs_reg));
            } else {
                panic!("Undefined identifier {ident}");
            }
        }
        _ => unreachable!(),
    }

    for op in iter {
        match op {
            StackExpr::Number(num) => {
                ctx.push_asm_line(&format!("SET {}, {:X}", _tmp, num));
            }
            StackExpr::Ident(ident) => {
                if ctx.var_is_global(ident) {
                    let rhs_addr = ctx.get_var_addr(ident).unwrap();
                    ctx.push_asm_line(&format!("LOAD {}, [{}]", _tmp, rhs_addr));
                } else if ctx.var_is_local(ident) {
                    let rhs_reg = ctx.get_register(ident).unwrap();
                    ctx.push_asm_line(&format!("MOV {}, {}", _tmp, rhs_reg));
                } else {
                    panic!("Undefined identifier {ident}");
                }
            }
            StackExpr::Add => {
                ctx.push_asm_line(&format!("ADD {}, {}", _acc, _tmp));
            }
            StackExpr::Sub => {
                ctx.push_asm_line(&format!("SUB {}, {}", _acc, _tmp));
            }
            StackExpr::Shl => todo!(),
            StackExpr::Shr => todo!(),
        }
    }
    if let Some(reg_dst) = ctx.get_register(&lhs) {
        // local variable assignment
        if reg_dst != _acc {
            ctx.push_asm_line(&format!("MOV {}, {}", reg_dst, _acc));
        }
    } else if let Some(dst_addr) = ctx.get_var_addr(&lhs) {
        ctx.push_asm_line(&format!("STR [{}], {}", dst_addr, _acc));
    }
    if acc_reg.is_none() {
        ctx.forget_register("_acc");
    }
    ctx.forget_register("_tmp");
}
