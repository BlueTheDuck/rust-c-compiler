use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;
use crate::{ast::{Expr, FuncDef, Program, Stmt, VarDecl, VarDef}, Res};

pub mod expr;

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
        } else if let Rule::EOI = pair.as_rule() {
            break;
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
                rhs: Expr::new(expr),
            })
        }
        Rule::stmt => parse_stmt(input.into_inner().next().unwrap()),
        Rule::var_decl => Ok(parse_var_decl(input)?.into()),
        Rule::var_def => Ok(parse_var_def(input)?.into()),
        Rule::label_def => Ok(Stmt::LabelDef(parse_label_def(input)?)),
        Rule::goto_label => Ok(Stmt::Goto(parse_goto_label(input)?)),
        _ => unreachable!("Unexpected rule: {:?}", input.as_rule()),
    }
}

// function was moved to its own module
use expr::parse_expr;

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
        Expr::new(parse_expr(expr)?),
    ))
}

fn parse_label_def(tokens: Pair<Rule>) -> Res<String> {
    debug_assert_eq!(tokens.as_rule(), Rule::label_def);
    let mut tokens = tokens.into_inner();
    let ident = tokens.next().unwrap(); // label
    debug_assert_eq!(ident.as_rule(), Rule::ident);
    Ok(ident.as_str().to_string())
}

fn parse_goto_label(tokens: Pair<Rule>) -> Res<String> {
    debug_assert_eq!(tokens.as_rule(), Rule::goto_label);
    let mut tokens = tokens.into_inner();
    let ident = tokens.next().unwrap(); // label
    debug_assert_eq!(ident.as_rule(), Rule::ident);
    Ok(ident.as_str().to_string())
}

#[cfg(test)]
mod tests {
    use crate::tokens::expr::StackExpr;
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
        let func_def = match statement {
            Stmt::FuncDef(func_def) => func_def,
            _ => panic!("Expected FuncDef, got {:?}", statement),
        };
        assert_eq!(func_def.ty, "void");
        assert_eq!(func_def.ident, "main");
        assert_eq!(func_def.args.len(), 2);
        assert_eq!(func_def.args[0].to_string(), "int x");
        assert_eq!(func_def.args[1].to_string(), "int y");
        assert_eq!(func_def.body.len(), 3);
        assert_eq!(func_def.body[0].to_string(), "int z;");
        assert_eq!(
            func_def.body[1],
            Stmt::Assignment {
                lhs: "x".to_string(),
                rhs: Expr::new(vec![StackExpr::Number(2)])
            }
        );
        assert_eq!(
            func_def.body[2],
            Stmt::VarDef(VarDef::new(
                &"int",
                &"w",
                Expr::new(vec![StackExpr::Number(3)])
            ))
        );
    }
}
