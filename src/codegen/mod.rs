mod analyzer;
mod context;
mod opcodes;

pub use analyzer::analyze_global;

use self::context::GlobalCtx;
pub fn compile(ctx: GlobalCtx) -> String {
    let mut asm = String::new();
    let blocks = ctx.functions();
    // first put main
    let code = blocks.get("main").expect("No main function found");
    asm.push_str("main:");
    asm.push_str(&code);

    for (path, code_block) in blocks.iter().filter(|(path, _)| path != &"main") {
        asm.push_str(&format!("\n\n{}:", path));
        asm.push_str(&code_block);
    }
    asm
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Stmt, tokens::parse};

    #[test]
    fn test_global() {
        let program: Vec<Stmt> = parse("int a;").unwrap();
        analyze_global(program);
    }

    #[test]
    fn test_assembly() {
        let program: Vec<Stmt> = parse("void main() {int x; x = 2;}").unwrap();
        let ctx = analyze_global(program);
        let code: Vec<&str> = ctx
            .functions()
            .get("main")
            .unwrap()
            .lines()
            .filter(|line| !line.starts_with(';'))
            .collect();
        assert_eq!(code[0], "SET R0, 2");
        assert_eq!(code.len(), 1);
    }
}
