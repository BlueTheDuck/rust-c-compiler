use crate::{
    ast::{Expr, Stmt, VarDef},
    codegen::{
        context::{CompilationCtx, Register},
        opcodes::*,
    },
    tokens::expr::StackExpr,
};

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
                ctx.push_asm_line(&format!("; {} = {}", ident, expr));
                compile_expression(ident, expr, ctx, Some(reg));
            }
            Stmt::VarDecl(var) => {
                // variable declarations can be either global or local
                // for global variables we assigna a memory location
                // for local variables we assign a register
                if ctx.is_global() {
                    ctx.push_asm_line(&format!("; {}", var.ident));
                    ctx.reserve_global_var(&var.ident);
                } else {
                    ctx.push_asm_line(&format!("; {}", var.ident));
                    ctx.reserve_register(&var.ident, None);
                }
            }
            Stmt::Assignment { lhs, rhs } => {
                // The assignment may use a variable both as a lhs and rhs
                // e.g.: x = x + 1
                // in that case we need to copy the value of x to a temporary register
                // otherwise we can just use the register of x as accumulator
                let self_assign = !rhs.list_idents().contains(&&lhs);
                if self_assign {
                    ctx.push_asm_line(&format!("; [self] {} = {}", lhs, rhs));
                    // If we are assigning to a variable that is also used as a rhs
                    // we let `compile_expression` reserve a temporary register for us
                    compile_expression(lhs, rhs, ctx, None);
                } else {
                    ctx.push_asm_line(&format!("; {} = {}", lhs, rhs));
                    // If we are NOT assigning to a variable that is also used as a rhs
                    // we can use it as the accumulator
                    let reg = ctx.get_register(&lhs);
                    compile_expression(lhs, rhs, ctx, reg);
                }
            }
            Stmt::LabelDef(ident) => {
                ctx.push_asm_line(&format!("{}:", ident));
            }
            Stmt::Goto(ident) => {
                ctx.push_asm_line(&format!("JMP {}", ident));
            }
            #[allow(unreachable_patterns)]
            _ => todo!("Stmt {stmt:#?}"),
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
            let rhs: Operand = match (ctx.get_register(ident), ctx.get_var_addr(ident)) {
                (Some(reg), None) => reg.into(),
                (None, Some(addr)) => addr.into(),
                (None, None) => {
                    panic!("Variable {ident} is not defined");
                }
                (Some(_), Some(_)) => {
                    unreachable!("Variable {ident} is defined both as a register and as an address")
                }
            };
            if let Some(opcode) = Op::emit_set(_acc.into(), rhs).unwrap() {
                ctx.push_asm_line(&format!("{}", opcode));
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
                let rhs: Operand = match (ctx.get_register(ident), ctx.get_var_addr(ident)) {
                    (Some(reg), None) => reg.into(),
                    (None, Some(addr)) => addr.into(),
                    (None, None) => {
                        panic!("Variable {ident} is not defined");
                    }
                    (Some(_), Some(_)) => unreachable!(
                        "Variable {ident} is defined both as a register and as an address"
                    ),
                };
                if let Some(opcode) = Op::emit_set(_acc.into(), rhs).unwrap() {
                    ctx.push_asm_line(&format!("{}", opcode));
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
