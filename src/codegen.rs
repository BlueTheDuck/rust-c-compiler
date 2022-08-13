use std::{collections::HashMap, num};

use crate::ast::{self, Stmt};

const GLOBAL_VARIABLE_START: usize = 0x1000;

type Scope = Vec<String>;

#[derive(Default)]
struct RegisterMap(Vec<(String, usize)>);
impl RegisterMap {
    pub fn new() -> Self {
        RegisterMap(Vec::new())
    }

    pub fn get(&self, name: &str) -> Option<usize> {
        self.0.iter().find(|(n, _)| n == name).map(|(_, r)| *r)
    }

    pub fn reserve(&mut self, name: &str, idx: Option<usize>) {
        assert!(
            self.get(name).is_none(),
            "Attempted to allocate a register for {name}, when it already has one"
        );
        let idx = match idx {
            Some(idx) => {
                assert!(
                    self.0.iter().find(|(n, r)| *r == idx).is_none(),
                    "Could not allocate register {idx} for {name} as it was already used"
                );
                idx
            }
            None => self
                .0
                .iter()
                .map(|(n, r)| *r)
                .max()
                .map(|addr| addr + 1)
                .unwrap_or(0),
        };
        self.0.push((name.to_string(), idx));
    }
}

#[derive(Default)]
pub struct CompilationCtx {
    scope_stack: Scope,
    var_map: HashMap<String, usize>,
    reg_maps: HashMap<String, RegisterMap>,
    code_blocks: HashMap<String, String>,
}
impl CompilationCtx {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn push_scope(&mut self, name: &str) {
        self.scope_stack.push(name.to_string());
        if self.reg_maps.get(&self.get_scope_path()).is_none() {
            self.reg_maps
                .insert(self.get_scope_path(), RegisterMap::new());
        }
    }
    pub fn pop_scope(&mut self) -> Option<String> {
        self.scope_stack.pop()
    }
    pub fn is_global(&self) -> bool {
        self.scope_stack.len() == 0
    }
    pub fn get_scope_path(&self) -> String {
        self.scope_stack.join("::")
    }

    pub fn reserve_global_var(&mut self, name: &str) {
        assert!(
            self.var_map.get(name).is_none(),
            "Variable '{}' already exists",
            name
        );
        let empty_addr = self
            .var_map
            .iter()
            .map(|(_, addr)| *addr)
            .max()
            .map(|addr| addr + 1)
            .unwrap_or(GLOBAL_VARIABLE_START);
        self.var_map.insert(name.to_string(), empty_addr);
    }
    pub fn get_var_addr(&self, name: &str) -> Option<usize> {
        self.var_map.get(name).map(|&addr| addr)
    }

    pub fn reserve_register(&mut self, name: &str, idx: Option<usize>) {
        assert!(!self.is_global());
        let regs = self.reg_maps.get_mut(&self.get_scope_path()).unwrap();
        regs.reserve(name, idx);
    }
    pub fn get_register(&self, name: &str) -> Option<usize> {
        assert!(!self.is_global());
        self.reg_maps.get(&self.get_scope_path()).unwrap().get(name)
    }

    pub fn var_is_global(&self, name: &str) -> bool {
        self.var_map.get(name).is_some()
    }
    pub fn var_is_local(&self, name: &str) -> bool {
        self.get_register(name).is_some()
    }

    pub fn push_asm_line(&mut self, line: &str) {
        let code_block = self.code_blocks.entry(self.get_scope_path()).or_default();
        code_block.push('\n');
        code_block.push_str(line);
    }

    pub fn get_asm(&self, path: Option<String>) {
        let path = path.unwrap_or_else(|| self.get_scope_path());
        println!("{}", self.code_blocks.get(&path).unwrap());
    }
}

pub fn analyze(program: Vec<Stmt>) -> CompilationCtx {
    let mut ctx = CompilationCtx::default();
    analyzer_scoped(program, &mut ctx);
    ctx
}

fn analyzer_scoped(program: Vec<Stmt>, ctx: &mut CompilationCtx) {
    for stmt in program {
        match stmt {
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
            Stmt::VarDef(_) => todo!(),
            Stmt::FuncDef(func) => {
                ctx.push_scope(&func.ident);
                for arg in func.args {
                    ctx.reserve_register(&arg.ident, None);
                }
                analyzer_scoped(func.body, ctx);
                ctx.pop_scope();
            }
            Stmt::Assignment { lhs, rhs } => {
                if let Some(reg_dst) = ctx.get_register(&lhs) {
                    // local variable assignment
                    match rhs {
                        ast::Expr::Literal(num) => {
                            let num: u8 = (num & 0xFF).try_into().expect("Numbers outside the 8bit range are not yet supported");
                            ctx.push_asm_line(&format!("SET R{}, {:02X}", reg_dst, num));
                        }
                        ast::Expr::Ident(ident) => {
                            if ctx.var_is_global(&ident) {
                                let addr = ctx.get_var_addr(&ident).unwrap();
                                ctx.push_asm_line(&format!("LOAD R{}, [{:X}]", reg_dst, addr));
                            } else if ctx.var_is_local(&ident) {
                                let reg_src = ctx.get_register(&ident).unwrap();
                                ctx.push_asm_line(&format!("MOV R{}, R{}", reg_dst, reg_src));
                            } else {
                                panic!("Undefined identifier '{ident}'");
                            }
                        }
                    }
                } else if let Some(dst_addr) = ctx.get_var_addr(&lhs) {
                    match rhs {
                        ast::Expr::Literal(num) => {
                            panic!("Cannot assign literal to global variable. The rhs for a global assign can only be a register");
                        }
                        ast::Expr::Ident(ident) => {
                            if let Some(src_reg) = ctx.get_register(&ident) {
                                ctx.push_asm_line(&format!("STR [{:X}], R{}", dst_addr, src_reg));
                            } else if let Some(src_addr) = ctx.get_var_addr(&ident) {
                                panic!("Cannot do globa-to-global assignment. The rhs for a global assign can only be a register");
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn compile(ctx: CompilationCtx) -> String {
    let mut asm = String::new();
    // first put main
    let code = ctx.code_blocks.get("main").expect("No main function found");
    asm.push_str("main:");
    asm.push_str(&code);

    for (path, code_block) in ctx.code_blocks.iter().filter(|(path, _)| path != &"main") {
        asm.push_str(&format!("{}:\n", path));
        asm.push_str(&code_block);
    }
    asm
}

#[cfg(test)]
mod tests {
    use crate::tokens::{parse, CParser, Rule};
    use pest::Parser;

    use super::*;

    #[test]
    fn test_compilation_ctx_reserve_global() {
        let mut ctx = CompilationCtx::new();
        ctx.reserve_global_var("a");
        ctx.reserve_global_var("b");
        ctx.reserve_global_var("c");
        assert_eq!(ctx.var_map.get("a"), Some(&GLOBAL_VARIABLE_START));
        assert_eq!(ctx.var_map.get("b"), Some(&(GLOBAL_VARIABLE_START + 1)));
        assert_eq!(ctx.var_map.get("c"), Some(&(GLOBAL_VARIABLE_START + 2)));
    }

    #[test]
    #[should_panic]
    fn test_compilation_ctx_reserve_global_again() {
        let mut ctx = CompilationCtx::new();
        ctx.reserve_global_var("a");
        ctx.reserve_global_var("a");
    }

    #[test]
    fn test_compilation_ctx_reserve_local() {
        let mut ctx = CompilationCtx::new();
        ctx.push_scope("main");
        ctx.reserve_register("a", None);
        ctx.reserve_register("b", None);
        ctx.reserve_register("c", None);
        assert_eq!(ctx.get_register("a"), Some(0));
        assert_eq!(ctx.get_register("b"), Some(1));
        assert_eq!(ctx.get_register("c"), Some(2));
        ctx.pop_scope();
        ctx.push_scope("foo");
        assert_eq!(ctx.get_register("a"), None);
        assert_eq!(ctx.get_register("b"), None);
        assert_eq!(ctx.get_register("c"), None);
    }

    #[test]
    #[should_panic]
    fn test_compilation_ctx_reserve_local_on_global() {
        let mut ctx = CompilationCtx::new();
        ctx.reserve_register("a", None);
    }

    #[test]
    fn test_global() {
        let program: Vec<Stmt> = parse("int a;").unwrap();
        analyze(program);
    }

    #[test]
    fn test_assembly() {
        let program: Vec<Stmt> = parse("void main() {int x; x = 2;}").unwrap();
        let ctx = analyze(program);
        ctx.get_asm(Some("main".to_string()));
    }
}
