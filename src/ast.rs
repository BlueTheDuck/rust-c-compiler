use core::fmt;

use itertools::Itertools;

use crate::tokens::expr::StackExpr;

pub type Program = Vec<Stmt>;

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    VarDecl(VarDecl),
    VarDef(VarDef),
    FuncDef(FuncDef),
    Assignment { lhs: String, rhs: Expr },
    Goto(String),
    LabelDef(String)
}
impl From<VarDecl> for Stmt {
    fn from(var: VarDecl) -> Self {
        Self::VarDecl(var)
    }
}
impl From<VarDef> for Stmt {
    fn from(var: VarDef) -> Self {
        Self::VarDef(var)
    }
}
impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VarDecl(var) => write!(f, "{ty} {ident};", ty = var.ty, ident = var.ident),
            Self::VarDef(var) => write!(
                f,
                "{ty} {ident} = {expr};",
                ty = var.ty,
                ident = var.ident,
                expr = var.expr
            ),
            Self::FuncDef(func) => write!(f, "{func}"),
            Self::Assignment { lhs, rhs } => write!(f, "{lhs} = {rhs};", lhs = lhs, rhs = rhs),
            Self::Goto(ident) => {
                write!(f, "goto {ident};", ident = ident)
            }
            Self::LabelDef(ident) => {
                write!(f, "{ident}:", ident = ident)
            }
            #[allow(unreachable_patterns)]
            _ => todo!("{:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub ty: String,
    pub ident: String,
}
impl VarDecl {
    pub fn new<S>(ty: &S, ident: &S) -> Self
    where
        S: ToString,
    {
        Self {
            ident: ident.to_string(),
            ty: ty.to_string(),
        }
    }
}
impl fmt::Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self.ident)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarDef {
    pub ty: String,
    pub ident: String,
    pub expr: Expr,
}
impl VarDef {
    pub fn new<S>(ty: &S, ident: &S, expr: Expr) -> Self
    where
        S: ToString,
    {
        Self {
            ident: ident.to_string(),
            expr,
            ty: ty.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncDef {
    pub ty: String,
    pub ident: String,
    pub args: Vec<VarDecl>,
    pub body: Vec<Stmt>,
}
impl fmt::Display for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{ty} {ident}({args}) {{{body}}}",
            ty = self.ty,
            ident = self.ident,
            args = self
                .args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", "),
            body = self
                .body
                .iter()
                .map(ToString::to_string)
                .collect::<String>()
        )
    }
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    /* pub span: Span, <- TODO: Seems like a good idea */
    pub stack: Vec<StackExpr>,
}
impl Expr {
    pub fn new(stack: Vec<StackExpr>) -> Self {
        Self { stack }
    }
    
    /**
     * List the identifiers used in this expression.
     */
    pub fn list_idents(&self) -> Vec<&String> {
        self.stack
            .iter()
            .filter_map(|expr| match expr {
                StackExpr::Ident(ident) => Some(ident),
                _ => None,
            })
            .collect()
    }
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.stack.iter().join(" "))
    }
}
