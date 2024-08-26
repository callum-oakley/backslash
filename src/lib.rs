#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

use anyhow::Result;
use sugar::Sugar;
use term::Term;

mod bytes;
mod int;
mod list;
mod sugar;
mod term;
mod test;

static STD: &str = include_str!("std.bs");

pub fn run(term: &str, input: &[u8]) -> Result<Vec<u8>> {
    bytes::decode(
        Term::app(
            Sugar::parse(&format!("{STD}\n{term}"))?.desugar()?,
            bytes::encode(input),
        )
        .reduce(),
    )
}

pub fn run_tests(term: &str) -> Result<()> {
    test::run(Sugar::parse(&format!("{STD}\n{term}"))?)
}
