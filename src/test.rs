use std::mem;

use anyhow::Result;
use lazy_static::lazy_static;

use crate::{sugar::Sugar, term::Term};

pub fn run(sugar: Sugar) -> Result<()> {
    lazy_static! {
        static ref TRUE: Term = Term::abs(Term::abs(Term::Var(1)));
    }
    let tests = collect_tests(sugar)?;
    for test in tests {
        assert_eq!(test.reduce(), TRUE.clone());
    }
    Ok(())
}

fn collect_tests(mut sugar: Sugar) -> Result<Vec<Term>> {
    let mut tests = Vec::new();
    while let Some((test, term)) = find_test(&sugar) {
        tests.push(substitute_test(&sugar, test).desugar()?);

        sugar = substitute_test(&sugar, term);
    }
    Ok(tests)
}

fn find_test<'a>(sugar: &Sugar<'a>) -> Option<(Sugar<'a>, Sugar<'a>)> {
    match sugar {
        Sugar::Abs(_, body) => find_test(body),
        Sugar::App(s, t) => find_test(s).or_else(|| find_test(t)),
        Sugar::Test(test, term) => Some((*test.clone(), *term.clone())),
        _ => None,
    }
}

fn substitute_test<'a>(sugar: &Sugar<'a>, mut substitution: Sugar<'a>) -> Sugar<'a> {
    fn swap_test<'a>(sugar: &mut Sugar<'a>, substitution: &mut Sugar<'a>) -> bool {
        match sugar {
            Sugar::Abs(_, body) => swap_test(body, substitution),
            Sugar::App(s, t) => swap_test(s, substitution) || swap_test(t, substitution),
            Sugar::Test(_, _) => {
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
