use self::context::CompilationCtx;

mod context;
mod analyzer;

pub use analyzer::analyze;
pub fn compile(ctx: CompilationCtx) -> String {
    let mut asm = String::new();
    let blocks = ctx.code_blocks();
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
    use crate::{tokens::parse, ast::Stmt};
    use super::*;


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
