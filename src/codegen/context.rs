use core::{fmt, ops};
use std::collections::HashMap;

const GLOBAL_VARIABLE_START: Address = Address(0x70);

type Scope = Vec<String>;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Default)]
#[repr(transparent)]
pub struct Register(pub u8);
impl std::ops::Deref for Register {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "R{}", self.0)
    }
}
impl From<u8> for Register {
    fn from(val: u8) -> Self {
        Self(val)
    }
}
impl From<Register> for u8 {
    fn from(val: Register) -> Self {
        val.0
    }
}
impl From<usize> for Register {
    fn from(val: usize) -> Self {
        assert!(val < 8);
        Self(val as u8)
    }
}
impl From<Register> for usize {
    fn from(val: Register) -> Self {
        val.0 as usize
    }
}
impl PartialEq<usize> for Register {
    fn eq(&self, other: &usize) -> bool {
        self.0 as usize == *other
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Default, PartialOrd, Ord)]
pub struct Address(pub u8);
impl std::ops::Deref for Address {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<I> ops::Add<I> for Address
where I: Into<u8> {
    type Output = Self;

    fn add(self, rhs: I) -> Self::Output {
        let add: _ = rhs.into();
        Self(self.0 + add)
    }
}
impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:02X}", self.0)
    }
}

#[derive(Default)]
struct RegisterMap([Option<String>; 8]);
impl RegisterMap {
    pub fn new() -> Self {
        RegisterMap([None, None, None, None, None, None, None, None])
    }

    pub fn get(&self, name: &str) -> Option<Register> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(i, reg)| reg.as_ref().map(|r| (i, r)))
            .find(|(_, n)| n == &name)
            .map(|(i, _)| Register(i as u8))
    }

    pub fn reserve(&mut self, name: &str, reg: Option<Register>) {
        assert!(
            self.get(name).is_none(),
            "Attempted to allocate a register for {name}, when it already has one"
        );
        let idx: usize = match reg {
            Some(reg) => {
                assert!(
                    self.0.get(usize::from(reg)).is_some(),
                    "Could not allocate register {reg} for {name} as it was already used"
                );
                reg.into()
            }
            None => self
                .0
                .iter()
                .enumerate()
                .find(|(_, name)| name.is_none())
                .map(|(idx, _)| Register::from(idx))
                .expect("Tried to reserve too many registers")
                .into(),
        };
        self.0[idx] = Some(name.to_string());
    }

    pub fn forget(&mut self, name: &str) {
        debug_assert!(
            self.get(name).is_some(),
            "Attempted to forget variable {name} which does not exist"
        );
        let idx: usize = self.get(name).unwrap().into();
        self.0[idx] = None;
    }
}

#[derive(Default)]
pub struct CompilationCtx {
    scope_stack: Scope,
    var_map: HashMap<String, Address>,
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

    pub fn reserve_global_var(&mut self, name: &str) -> Address {
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
        return empty_addr;
    }
    pub fn get_var_addr(&self, name: &str) -> Option<Address> {
        self.var_map.get(name).map(|&addr| addr)
    }

    pub fn reserve_register(&mut self, name: &str, idx: Option<Register>) -> Register {
        assert!(!self.is_global());
        let regs = self.reg_maps.get_mut(&self.get_scope_path()).unwrap();
        regs.reserve(name, idx);
        return regs.get(name).unwrap();
    }
    pub fn get_register(&self, name: &str) -> Option<Register> {
        assert!(!self.is_global());
        self.reg_maps.get(&self.get_scope_path()).unwrap().get(name)
    }
    pub fn forget_register(&mut self, name: &str) {
        assert!(!self.is_global());
        self.reg_maps
            .get_mut(&self.get_scope_path())
            .unwrap()
            .forget(name);
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

    pub fn code_blocks(&self) -> &HashMap<String, String> {
        &self.code_blocks
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ctx_reserve_global() {
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
    fn test_ctx_reserve_global_again() {
        let mut ctx = CompilationCtx::new();
        ctx.reserve_global_var("a");
        ctx.reserve_global_var("a");
    }

    #[test]
    fn test_ctx_reserve_local() {
        let mut ctx = CompilationCtx::new();
        ctx.push_scope("main");
        ctx.reserve_register("a", None);
        ctx.reserve_register("b", None);
        ctx.reserve_register("c", None);
        assert_eq!(ctx.get_register("a"), Some(Register::from(0u8)));
        assert_eq!(ctx.get_register("b"), Some(Register::from(1u8)));
        assert_eq!(ctx.get_register("c"), Some(Register::from(2u8)));
        ctx.pop_scope();
        ctx.push_scope("foo");
        assert_eq!(ctx.get_register("a"), None);
        assert_eq!(ctx.get_register("b"), None);
        assert_eq!(ctx.get_register("c"), None);
    }

    #[test]
    #[should_panic]
    fn test_ctx_reserve_local_on_global() {
        let mut ctx = CompilationCtx::new();
        ctx.reserve_register("a", None);
    }

    #[test]
    #[should_panic]
    fn test_ctx_reserve_twice() {
        let mut ctx = CompilationCtx::new();
        ctx.reserve_register("a", None);
        ctx.reserve_register("a", None);
    }

    #[test]
    #[should_panic]
    fn test_ctx_reserve_too_many_locals() {
        let mut ctx = CompilationCtx::new();
        ctx.push_scope("main");
        for i in 0..=7 {
            ctx.reserve_register(&format!("reg-{i}"), None);
        }
        for i in 0..=7 {
            let reg = ctx.get_register(&format!("reg-{i}"));
            assert_eq!(reg, Some(Register(i)));
        }
        ctx.reserve_register("one-too-many", None);
    }
}
