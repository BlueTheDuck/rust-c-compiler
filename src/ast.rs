use crate::token::{Span, Token, TokenType};
use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Eq)]
enum AstError {
    Expected(String),
    Eof,
}
impl AstError {
    fn new_expected(expected: &str, span: &Span, found: &dyn core::fmt::Debug) -> Self {
        let line = span.location_line();
        let col = span.get_column();
        let msg = format!("Expected {expected} at {line}:{col}. Found {found:#?}");
        Self::Expected(msg.into())
    }
}

struct TokenStream<'i> {
    tokens: &'i [Token<'i>],
    index: usize,
}
impl<'i> TokenStream<'i> {
    pub fn new(tokens: &'i [Token]) -> Self {
        Self { tokens, index: 0 }
    }
    pub fn back(&mut self) {
        self.index -= 1;
    }
    
    pub fn pop_type(&mut self) -> Result<CType, AstError> {
        let token = &self.tokens[self.index];
        self.index += 1;
        match token.kind {
            TokenType::Int => Ok(CType::Int),
            TokenType::Void => Ok(CType::Void),
            _ => Err(AstError::new_expected("C Type", &token.span, &token.kind)),
        }
    }
    pub fn pop_ident(&mut self) -> Result<&'i str, AstError> {
        let token = &self.tokens[self.index];
        self.index += 1;
        match token.kind {
            TokenType::Ident(ident) => Ok(ident),
            _ => Err(AstError::new_expected("Ident", &token.span, &token.kind)),
        }
    }
    pub fn pop_int_lit(&mut self) -> Result<i64, AstError> {
        let token = &self.tokens[self.index];
        self.index += 1;
        match token.kind {
            TokenType::IntLiteral(int) => Ok(int),
            _ => Err(AstError::new_expected(
                "Int Literal (Hex or decimal)",
                &token.span,
                &token.kind,
            )),
        }
    }
    pub fn pop_semi_colon(&mut self) -> Result<(), AstError> {
        let token = &self.tokens[self.index];
        self.index += 1;
        match token.kind {
            TokenType::SemiColon => Ok(()),
            _ => Err(AstError::new_expected(
                "Semi-colon (;)",
                &token.span,
                &token.kind,
            )),
        }
    }
    pub fn pop_symbol(&mut self, sym: &str) -> Result<(), AstError> {
        let token = &self.tokens[self.index];
        self.index += 1;
        let token_str = format!("{}", token.kind);
        if token_str == sym {
            Ok(())
        } else {
            Err(AstError::new_expected(sym, &token.span, &token.kind))
        }
    }
    pub fn peek_ident(&self) -> Option<&'i str> {
        self.tokens
            .get(self.index)
            .filter(|token| {
                if let TokenType::Ident(ident) = token.kind {
                    true
                } else {
                    false
                }
            })
            .map(|token| match token.kind {
                TokenType::Ident(ident) => ident,
                _ => unreachable!(),
            })
    }
    pub fn peek_int_lit(&self) -> Option<i64> {
        self.tokens
            .get(self.index)
            .filter(|token| {
                if let TokenType::IntLiteral(ident) = token.kind {
                    true
                } else {
                    false
                }
            })
            .map(|token| match token.kind {
                TokenType::IntLiteral(int) => int,
                _ => unreachable!(),
            })
    }
    pub fn peek_symbol(&self, sym: &str) -> bool {
        self.tokens
            .get(self.index)
            .map(|token| token.kind)
            .filter(TokenType::is_symbol)
            .filter(|kind| {
                let disp = format!("{kind}");
                disp == sym
            })
            .is_some()
    }
    pub fn peek_ctype(&self) -> bool {
        self.tokens
            .get(self.index)
            .map(|token| token.kind)
            .filter(TokenType::is_ctype)
            .is_some()
    }
}
type AstResult<'i, T> = Result<(TokenStream<'i>, T), AstError>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CType {
    Void,
    Int,
}
struct VarDecl<'i> {
    pub name: &'i str,
    pub ty: CType,
}
fn build_var_decl<'i>(mut input: TokenStream<'i>) -> AstResult<VarDecl> {
    let ty = input.pop_type()?;
    let ident = input.pop_ident()?;
    Ok((input, VarDecl { name: ident, ty }))
}

struct FuncDecl<'i> {
    pub name: &'i str,
    pub params: Vec<VarDecl<'i>>,
    pub return_ty: CType,
    pub code: Vec<Stmt<'i>>
}
fn build_func_decl<'i>(mut input: TokenStream<'i>) -> AstResult<FuncDecl<'i>> {
    let name = input.pop_ident()?;
    build_var_decl(input)?;
    todo!()
}

pub enum Stmt<'i> {
    FuncDecl(FuncDecl<'i>),
    VarDecl(VarDecl<'i>),
    Assigment { ident: &'i str, value: Expr<'i> },
}
fn build_stmt<'i>(mut input: TokenStream<'i>) -> AstResult<Stmt> {
    /*
    int x;
    x = 1;
    */
    if input.peek_ctype() {
        /* Declaration */
        let (mut input, decl) = build_var_decl(input)?;
        input.pop_semi_colon()?;
        Ok((input, Stmt::VarDecl(decl)))
    } else if input.peek_ident().is_some() {
        /* Assigment */
        let ident = input.pop_ident()?;
        input.pop_symbol("=")?;
        let (mut input, value) = build_expr(input)?;
        input.pop_semi_colon()?;
        Ok((input, Stmt::Assigment { ident, value }))
    } else {
        todo!("other stmt types are not implemented yet")
    }
    /* let ident = input.pop_ident()?;
    if input.peek_symbol("=") {
        let (mut input, expr) = build_expr(input)?;
        Ok((input, Stmt::Assigment { ident, value: expr }))
    } else if input. */
}

#[derive(Debug, PartialEq, Eq)]
enum Expr<'i> {
    LiteralInt(i64),
    Ident(&'i str),
    Add(Box<Expr<'i>>, Box<Expr<'i>>),
}
fn build_expr<'i>(mut input: TokenStream<'i>) -> AstResult<Expr> {
    if input.peek_ident().is_some() {
        let ident = input.pop_ident()?;
        Ok((input, Expr::Ident(ident)))
    } else if input.peek_int_lit().is_some() {
        let int = input.pop_int_lit()?;
        Ok((input, Expr::LiteralInt(int)))
    } else {
        todo!("other expr types are not implemented yet")
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::*,
        token::{tokenize, Span},
    };

    #[test]
    fn test_var_decl() {
        let (_, tokens) = tokenize(Span::new("int x;")).unwrap();
        let (_, var) = build_var_decl(TokenStream::new(tokens.as_slice())).unwrap();
        assert_eq!(var.name, "x");
        assert_eq!(var.ty, CType::Int);
    }

    #[test]
    fn test_assigment() {
        let (_, tokens) = tokenize(Span::new("x = 1;")).unwrap();
        let (_, stmt) = build_stmt(TokenStream::new(tokens.as_slice())).unwrap();
        if let Stmt::Assigment { ident, value } = stmt {
            assert_eq!(ident, "x");
            assert_eq!(value, Expr::LiteralInt(1));
        }
    }

}
