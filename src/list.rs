use anyhow::{ensure, Result};

use crate::term::Term;

pub fn encode(mut terms: impl Iterator<Item = Term>) -> Term {
    if let Some(term) = terms.next() {
        cons(term, encode(terms))
    } else {
        new_nil()
    }
}

pub fn decode(mut term: Term) -> Result<Vec<Term>> {
    let mut terms = Vec::new();
    while term != new_nil() {
        term = term.try_unabs()?.try_unabs()?;
        let (ifcons_head, tail) = term.try_unapp()?;
        let (ifcons, head) = ifcons_head.try_unapp()?;
        ensure!(ifcons == Term::Var(0), "not a list");
        terms.push(head);
        term = tail;
    }
    Ok(terms)
}

fn new_nil() -> Term {
    Term::new(r"\ifnil ifcons.ifnil").unwrap()
}

fn new_cons() -> Term {
    Term::new(r"\head tail ifnil ifcons.ifcons head tail").unwrap()
}

fn cons(a: Term, b: Term) -> Term {
    Term::app(Term::app(new_cons(), a), b).reduce()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list() {
        let terms: Vec<Term> = (0..10).map(crate::int::encode).collect();
        assert_eq!(decode(encode(terms.clone().into_iter())).unwrap(), terms);
    }
}
