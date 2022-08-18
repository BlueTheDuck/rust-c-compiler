use core::{fmt, ops};
use std::collections::HashMap;

const GLOBAL_VARIABLE_START: Address = Address(0x70);

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
where
    I: Into<u8>,
{
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

    pub fn reserve(&mut self, name: &str, reg: Option<Register>) -> Register {
        assert!(
            self.get(name).is_none(),
            "Attempted to allocate a register for '{name}', when it already has one"
        );
        let idx: usize = match reg {
            Some(reg) => {
                assert!(
                    self.0.get(usize::from(reg)).is_some(),
                    "Could not allocate register {reg} for '{name}' as it was already used"
                );
                reg.into()
            }
            None => self
                .0
                .iter()
                .enumerate()
                .find(|(_, name)| name.is_none())
                .map(|(idx, _)| Register::from(idx))
                .expect(&format!(
                    "Can't allocate register for '{name}'. Too many registers are already in use"
                ))
                .into(),
        };
        self.0[idx] = Some(name.to_string());
        return idx.into();
    }

    pub fn forget(&mut self, name: &str) {
        debug_assert!(
            self.get(name).is_some(),
            "Attempted to forget variable '{name}' which does not exist"
        );
        let idx: usize = self.get(name).unwrap().into();
        self.0[idx] = None;
    }
}

#[derive(Default)]
pub struct GlobalCtx {
    var_map: HashMap<String, Address>,
    functions: HashMap<String, String>,
}
impl GlobalCtx {
    pub fn in_function<F>(&mut self, name: &str, f: F)
    where
        F: FnOnce(&mut FunctionCtx),
    {
        assert!(self.functions.get(name).is_none(), "Function {name} already exists");
        let mut scope = FunctionCtx::new(name.to_string(), self);
        f(&mut scope);
        let FunctionCtx { code, .. } = scope;
        self.functions.insert(name.to_string(), code);
    }

    pub fn reserve_global_var(&mut self, name: &str) -> Address {
        assert!(
            self.var_map.get(name).is_none(),
            "Attempted to reserve global variable '{name}' when it already exists"
        );
        let addr = self
            .var_map
            .iter()
            .max()
            .map(|(_, &addr)| addr + 1)
            .unwrap_or(GLOBAL_VARIABLE_START);
        self.var_map.insert(name.to_string(), addr);
        addr
    }
    pub fn get_global_var(&self, name: &str) -> Option<Address> {
        self.var_map.get(name).cloned()
    }

    pub fn functions(&self) -> &HashMap<String, String> {
        &self.functions
    }
}

pub struct FunctionCtx<'g> {
    name: String,
    global: &'g GlobalCtx,
    registers: RegisterMap,
    local_counter: usize,
    code: String,
}

impl<'g> FunctionCtx<'g> {
    fn new(name: String, global: &'g GlobalCtx) -> Self {
        Self {
            name,
            global,
            registers: RegisterMap::new(),
            local_counter: 0,
            code: String::new(),
        }
    }

    pub fn address(&self, name: &str) -> Option<Address> {
        self.global.get_global_var(name)
    }

    pub fn reserve_local(&mut self, name: &str) -> Register {
        self.registers.reserve(name, None)
    }
    pub fn reserve_register(&mut self, name: &str, register: Register) {
        self.registers.reserve(name, Some(register));
    }
    pub fn register(&self, name: &str) -> Option<Register> {
        self.registers.get(name)
    }
    pub fn forget_register(&mut self, name: &str) {
        self.registers.forget(name);
    }
    pub fn with_tmp<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self, Register),
    {
        let reg_name = format!("tmp{}", self.local_counter);
        let tmp = self.reserve_local(&reg_name);
        self.local_counter += 1;
        f(self, tmp);
        self.forget_register(&reg_name);
    }

    pub fn push_asm_line<S>(&mut self, line: S)
    where S: AsRef<str> {
        self.code.push_str(line.as_ref());
        self.code.push('\n');
    }

    pub fn gen_local_label(&mut self, label: &str) -> String {
        let label = format!(
            "{name}.{label}.L{idx}",
            name = self.name,
            idx = self.local_counter
        );
        self.local_counter += 1;
        label
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global_reserve_global() {
        let mut ctx: GlobalCtx = Default::default();
        ctx.reserve_global_var("a");
        ctx.reserve_global_var("b");
        ctx.reserve_global_var("c");
        assert!(ctx.get_global_var("a").is_some());
        assert!(ctx.get_global_var("b").is_some());
        assert!(ctx.get_global_var("c").is_some());
        let a = ctx.get_global_var("a").unwrap();
        let b = ctx.get_global_var("b").unwrap();
        let c = ctx.get_global_var("c").unwrap();
        assert_ne!(a, b);
        assert_ne!(a, c);
        assert_ne!(b, c);
    }

    #[test]
    #[should_panic]
    fn test_global_reserve_global_again() {
        let mut ctx: GlobalCtx = Default::default();
        ctx.reserve_global_var("a");
        ctx.reserve_global_var("a");
    }

    #[test]
    #[should_panic]
    fn test_global_ctx_same_function() {
        let mut ctx: GlobalCtx = Default::default();
        ctx.in_function("main", |_| {});
        ctx.in_function("main", |_| {});
    }

    #[test]
    fn test_local_ctx_reserve_local() {
        let mut ctx: GlobalCtx = Default::default();
        ctx.in_function("main", |ctx| {
            let a = ctx.reserve_local("a");
            let b = ctx.reserve_local("b");
            let c = ctx.reserve_local("c");
            assert!(a.0 < 8);
            assert!(b.0 < 8);
            assert!(c.0 < 8);
        });
        ctx.in_function("irq", |ctx| {
            let a = ctx.reserve_local("a");
            let b = ctx.reserve_local("b");
            let c = ctx.reserve_local("c");
            assert!(a.0 < 8);
            assert!(b.0 < 8);
            assert!(c.0 < 8);
        });
    }

    #[test]
    #[should_panic = "Attempted to allocate a register for 'a', when it already has one"]
    fn test_local_ctx_reserve_local_again() {
        let mut ctx: GlobalCtx = Default::default();
        ctx.in_function("main", |ctx| {
            let _ = ctx.reserve_local("a");
            let _ = ctx.reserve_local("a");
        });
    }

    #[test]
    #[should_panic = "Can't allocate register for 'one_to_many'. Too many registers are already in use"]
    fn test_local_ctx_reserve_too_many() {
        let mut ctx: GlobalCtx = Default::default();
        ctx.in_function("main", |ctx| {
            for i in 0..8 {
                ctx.reserve_local(&format!("a{i}"));
            }
            ctx.reserve_local("one_to_many");
        });
    }

    #[test]
    fn test_local_ctx_counter() {
        let mut ctx: GlobalCtx = Default::default();
        ctx.in_function("main", |ctx| {
            let label = ctx.gen_local_label("label");
            assert!(ctx.local_counter == 1);
        });
    }
}
