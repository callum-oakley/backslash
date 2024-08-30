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
        term = term.decode_abs()?.decode_abs()?;
        let (var0_head, tail) = term.decode_app()?;
        let (var0, head) = var0_head.decode_app()?;
        ensure!(var0 == Term::Var(0), "decoding cons");
        terms.push(head);
        term = tail;
    }
    Ok(terms)
}

fn new_nil() -> Term {
    Term::abs(Term::abs(Term::Var(1)))
}

fn cons(a: Term, b: Term) -> Term {
    Term::abs(Term::abs(Term::app(Term::app(Term::Var(0), a), b)))
}
