use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

use crate::ast::{Expr, FuncDef, Program, Stmt, VarDecl, VarDef};

type Res<T> = Result<T, Box<dyn std::error::Error>>;

#[derive(Parser, Debug)]
#[grammar = "grammar.pest"]
pub struct CParser;

#[allow(dead_code)]
pub fn parse(input: &str) -> Res<Program> {
    let parsed: Pairs<Rule> = CParser::parse(Rule::program, input)?;
    let mut ast: Program = Default::default();
    for pair in parsed {
        if let Rule::var_decl = pair.as_rule() {
            // ty var;
            // let mut input = pair.into_inner();
            let var_decl = parse_var_decl(pair)?.into();
            ast.push(var_decl);
        } else if let Rule::func_def = pair.as_rule() {
            let mut input: Pairs<_> = pair.into_inner();
            let ty = input.next().unwrap();
            let ident = input.next().unwrap();
            let mut args: Vec<VarDecl> = vec![];
            loop {
                if input.peek().filter(|p| p.as_rule() == Rule::stmt).is_some() {
                    break;
                } else if let Some(arg) = input.next() {
                    assert_eq!(arg.as_rule(), Rule::var_decl);
                    args.push(parse_var_decl(arg)?.into());
                } else {
                    break;
                }
            }
            let mut stmts: Vec<Stmt> = vec![];
            loop {
                if let Some(stmt) = input.next() {
                    assert_eq!(stmt.as_rule(), Rule::stmt);
                    stmts.push(parse_stmt(stmt.into_inner().next().unwrap())?.into());
                } else {
                    break;
                }
            }
            assert_eq!(input.peek(), None, "Unexpeced tokens after function body");
            let func = Stmt::FuncDef(FuncDef {
                ty: ty.as_str().to_string(),
                ident: ident.as_str().to_string(),
                args,
                body: stmts,
            });
            ast.push(func);
        } else {
            panic!("Unexpected rule {:#?}", pair);
        }
    }
    Ok(ast)
}

fn parse_stmt(input: Pair<Rule>) -> Res<Stmt> {
    match input.as_rule() {
        Rule::ret => todo!(),
        Rule::assignment => {
            let mut input = input.into_inner();
            let ident = input.next().unwrap();
            let expr = parse_expr(input.next().unwrap())?;
            Ok(Stmt::Assignment {
                lhs: ident.as_str().to_string(),
                rhs: expr,
            })
        }
        Rule::stmt => parse_stmt(input.into_inner().next().unwrap()),
        Rule::var_decl => Ok(parse_var_decl(input)?.into()),
        Rule::var_def => Ok(parse_var_def(input)?.into()),
        _ => unreachable!(),
    }
}

fn parse_expr(expr: Pair<Rule>) -> Res<Expr> {
    // assert_eq!(expr.as_rule(), Rule::expr);
    match expr.as_rule() {
        Rule::num => {
            let num = parse_num(expr.as_str())?;
            Ok(Expr::Literal(num))
        }
        Rule::ident => Ok(Expr::Ident(expr.as_str().to_string())),
        Rule::expr => {
            let expr = expr.into_inner().next().unwrap();
            parse_expr(expr)
        }
        _ => unreachable!("{:#?} is not an expr", expr.as_rule()),
    }
}
fn parse_num(num_str: &str) -> Res<i64> {
    let num;
    if num_str.starts_with("0x") {
        num = i64::from_str_radix(&num_str[2..], 16)?;
    } else if num_str.starts_with("0b") {
        num = i64::from_str_radix(&num_str[2..], 2)?;
    } else {
        num = i64::from_str_radix(num_str, 10)?;
    }
    return Ok(num);
}

fn parse_var_decl(input: Pair<Rule>) -> Res<VarDecl> {
    debug_assert!(input.as_rule() == Rule::var_decl);
    let mut tokens = input.into_inner();
    let ty = tokens.next().unwrap();
    let ident = tokens.next().unwrap();
    Ok(VarDecl::new(&ty.as_str(), &ident.as_str()))
}

fn parse_var_def(input: Pair<Rule>) -> Res<VarDef> {
    debug_assert!(input.as_rule() == Rule::var_def);
    let mut tokens = input.into_inner();
    let ty = tokens.next().unwrap();
    let ident = tokens.next().unwrap();
    let expr = tokens.next().unwrap();
    Ok(VarDef::new(
        &ty.as_str(),
        &ident.as_str(),
        parse_expr(expr)?,
    ))
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::*;

    #[test]
    fn test_func_def() {
        let input = "void main() {}";
        let program = parse(input).unwrap();
        assert_eq!(program.len(), 1);
        let func = program.get(0).unwrap();
        if let Stmt::FuncDef(func) = func {
            assert_eq!(func.ty, "void");
            assert_eq!(func.ident, "main");
            assert_eq!(func.args.len(), 0);
            assert_eq!(func.body.len(), 0);
        } else {
            panic!("Expected FuncDef, got {:?}", func);
        }
    }

    #[test]
    fn test_func_def_args_body() {
        let input = "void main(int x, int y) {int z;x = 2;int w = 3;}";
        let program = parse(input).unwrap();
        assert_eq!(program.len(), 1);
        let statement = program.get(0).unwrap();
        match statement {
            Stmt::FuncDef(func) => {
                assert_eq!(func.ident, "main");
                assert_eq!(func.args.len(), 2);
                assert_eq!(func.args[0], VarDecl::new(&"int", &"x"));
                assert_eq!(func.args[1], VarDecl::new(&"int", &"y"));
                assert_eq!(func.body.len(), 3);
                match func.body.get(0).unwrap() {
                    Stmt::VarDecl(var_decl) => assert_eq!(var_decl, &VarDecl::new(&"int", &"z")),
                    _ => panic!("Unexpected statement {:#?}", func.body.get(0).unwrap()),
                }
                match func.body.get(1).unwrap() {
                    Stmt::Assignment { lhs, rhs } => {
                        assert_eq!(lhs, "x");
                        assert_eq!(rhs, &Expr::Literal(2));
                    }
                    _ => panic!("Unexpected statement {:#?}", func.body.get(1).unwrap()),
                }
                match func.body.get(2).unwrap() {
                    Stmt::VarDef(var_decl) => {
                        assert_eq!(var_decl, &VarDef::new(&"int", &"w", Expr::Literal(3)))
                    }
                    _ => panic!("Unexpected statement {:#?}", func.body.get(2).unwrap()),
                }
            }
            _ => panic!("Expected function definition"),
        }
    }
}
