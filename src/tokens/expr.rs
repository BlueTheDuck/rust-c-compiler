use itertools::Itertools;
use pest::iterators::Pair;
use crate::{Res, tokens::Rule};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StackExpr {
    Number(i64),
    Ident(String),
    Add,
    Sub,
    Shl,
    Shr,
}
impl From<Pair<'_, Rule>> for StackExpr {
    fn from(token: Pair<Rule>) -> Self {
        match token.as_rule() {
            Rule::num => StackExpr::Number(parse_num(token.as_str()).unwrap()),
            Rule::ident => StackExpr::Ident(token.as_str().to_string()),
            Rule::bin_op_1 | Rule::bin_op_2 => match token.as_str() {
                "+" => StackExpr::Add,
                "-" => StackExpr::Sub,
                "<<" => StackExpr::Shl,
                ">>" => StackExpr::Shr,
                _ => unreachable!(),
            },
            Rule::expr_2 => panic!("Don't pass expr_2 to this function"),
            _ => unreachable!("Unexpected rule {:?}", token.as_rule()),
        }
    }
}
impl core::fmt::Display for StackExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackExpr::Number(num) => write!(f, "{num}"),
            StackExpr::Ident(ident) => write!(f, "{ident}"),
            StackExpr::Add => write!(f, "+"),
            StackExpr::Sub => write!(f, "-"),
            StackExpr::Shl => write!(f, "<<"),
            StackExpr::Shr => write!(f, ">>"),
            #[allow(unreachable_patterns)]
            _ => todo!("I forgor to impl {self:#?}"),
        }
    }
}

fn parse_expr_2(tokens: Pair<Rule>, stack: &mut Vec<StackExpr>) {
    assert_eq!(tokens.as_rule(), Rule::expr_2);
    let mut tokens = tokens.into_inner();
    let first = tokens.next().unwrap();
    stack.push(first.into());
    for token_pair in tokens.chunks(2).into_iter() {
        let (op, val) = token_pair.collect_tuple().unwrap();
        stack.push(val.into());
        stack.push(op.into());
    }
}

pub fn parse_expr(tokens: Pair<Rule>) -> Res<Vec<StackExpr>> {
    assert_eq!(
        tokens.as_rule(),
        Rule::expr,
        "Called parse_expr with {:?}",
        tokens.as_rule()
    );

    let mut stack = vec![];
    let mut tokens = tokens.into_inner();

    // We always have at least one expr_2
    let first = tokens.next().unwrap();
    parse_expr_2(first, &mut stack);

    // The rest of the tokens are an OP followed by an expr_2.
    // we can iterate over the tokens in pairs because we know that there are an even number of tokens.
    // e. g.: 1 + 2 + 3 -> 1 [+ 2] +3 -> 1 + 2 [+ 3]
    for token_pair in tokens.chunks(2).into_iter() {
        let (op, val) = token_pair.collect_tuple().unwrap();
        assert_eq!(val.as_rule(), Rule::expr_2);
        parse_expr_2(val, &mut stack);
        stack.push(op.into());
    }

    return Ok(stack);
}

pub fn parse_num(num_str: &str) -> Res<i64> {
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

#[cfg(test)]
mod tests {
    use pest::{iterators::Pairs, Parser};
    use crate::tokens::{Rule, CParser};
    use super::*;

    #[test]
    fn expr_to_stack() {
        let input = "x<<1 + 2 + 2 + x<<1";
        // output: S(x<<1), S(2), +, S(2), +, S(x<<1), +
        // Output: (x 1 <<), (2), (+), (2), (+), (x, 1, <<), +

        let mut tokens: Pairs<Rule> = CParser::parse(Rule::expr, input).unwrap();
        let stack = parse_expr(tokens.next().unwrap()).unwrap();

        assert_eq!(
            stack.len(),
            11,
            "Stack has {} elements: \n  {}",
            stack.len(),
            stack.into_iter().join(" ")
        );
        let stack_str = stack.into_iter().join(" ");
        assert_eq!(stack_str, "x 1 << 2 + 2 + x 1 << +");
    }
}
