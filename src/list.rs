use anyhow::{ensure, Result};

use crate::term::Bruijn;

pub fn encode(mut terms: impl Iterator<Item = Bruijn>) -> Bruijn {
    if let Some(term) = terms.next() {
        cons(term, encode(terms))
    } else {
        new_nil()
    }
}

pub fn decode(mut term: Bruijn) -> Result<Vec<Bruijn>> {
    let mut terms = Vec::new();
    while term != new_nil() {
        term = term.try_unabs()?.try_unabs()?;
        let (ifcons_head, tail) = term.try_unapp()?;
        let (ifcons, head) = ifcons_head.try_unapp()?;
        ensure!(ifcons == Bruijn::Var(0), "not a list");
        terms.push(head);
        term = tail;
    }
    Ok(terms)
}

fn new_nil() -> Bruijn {
    Bruijn::new(r"\ifnil ifcons.ifnil").unwrap()
}

fn new_cons() -> Bruijn {
    Bruijn::new(r"\head tail ifnil ifcons.ifcons head tail").unwrap()
}

fn cons(a: Bruijn, b: Bruijn) -> Bruijn {
    Bruijn::app(Bruijn::app(new_cons(), a), b).reduce()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list() {
        let terms: Vec<Bruijn> = (0..10).map(crate::int::encode).collect();
        assert_eq!(decode(encode(terms.clone().into_iter())).unwrap(), terms);
    }
}
