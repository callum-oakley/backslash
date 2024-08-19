#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

use anyhow::Result;
use term::Term;

mod bytes;
mod int;
mod list;
mod parser;
mod term;

static STD: &str = include_str!("std.bs");

pub fn run(term: &str, input: &[u8]) -> Result<Vec<u8>> {
    bytes::decode(Term::app(Term::new(&format!("{STD}\n{term}"))?, bytes::encode(input)).reduce())
}
