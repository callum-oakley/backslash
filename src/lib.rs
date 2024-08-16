#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

use anyhow::Result;
use term::Bruijn;

mod io;
mod parser;
mod term;

static STD: &str = include_str!("std.bs");

pub fn run(term: &str, input: &[u8]) -> Result<Vec<u8>> {
    let mut full_term = STD.to_owned();
    full_term.push_str(term);
    Bruijn::app(Bruijn::new(&full_term)?, input.into())
        .reduce()
        .try_into()
}
