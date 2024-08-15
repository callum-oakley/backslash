#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

use anyhow::Result;
use term::Bruijn;

mod constants;
mod io;
mod parser;
mod term;

pub fn run(term: &str, input: &[u8]) -> Result<Vec<u8>> {
    Bruijn::app(Bruijn::new(term)?, input.into())
        .reduce()
        .try_into()
}
