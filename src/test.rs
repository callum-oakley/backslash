use std::mem;

use anyhow::{anyhow, bail, Result};
use lazy_static::lazy_static;

use crate::{
    sugar::{OffsetError, Sugar},
    term::Term,
};

pub fn run(sugar: Sugar) -> Result<()> {
    lazy_static! {
        static ref TRUE: Term = Term::abs(Term::abs(Term::Var(1)));
    }
    let tests = collect_tests(sugar)?;
    for (test, offset) in tests {
        if test.reduce() != TRUE.clone() {
            bail!(OffsetError::new(offset, anyhow!("test failure")));
        }
    }
    Ok(())
}

fn collect_tests(mut sugar: Sugar) -> Result<Vec<(Term, usize)>> {
    let mut tests = Vec::new();
    while let Some((test, term, offset)) = find_test(&sugar) {
        tests.push((substitute_test(&sugar, test).desugar()?, offset));

        sugar = substitute_test(&sugar, term);
    }
    Ok(tests)
}

fn find_test<'a>(sugar: &Sugar<'a>) -> Option<(Sugar<'a>, Sugar<'a>, usize)> {
    match sugar {
        Sugar::Abs(_, body, _) => find_test(body),
        Sugar::App(s, t, _) => find_test(s).or_else(|| find_test(t)),
        Sugar::Test(test, term, offset) => Some((*test.clone(), *term.clone(), *offset)),
        _ => None,
    }
}

fn substitute_test<'a>(sugar: &Sugar<'a>, mut substitution: Sugar<'a>) -> Sugar<'a> {
    fn swap_test<'a>(sugar: &mut Sugar<'a>, substitution: &mut Sugar<'a>) -> bool {
        match sugar {
            Sugar::Abs(_, body, _) => swap_test(body, substitution),
            Sugar::App(s, t, _) => swap_test(s, substitution) || swap_test(t, substitution),
            Sugar::Test(..) => {
                mem::swap(sugar, substitution);
                true
            }
            _ => false,
        }
    }
    let mut sugar = sugar.clone();
    assert!(swap_test(&mut sugar, &mut substitution));
    sugar
}
