use std::{
    env::args,
    io::{self, Write},
};

use backslash::term::Bruijn;

fn main() -> anyhow::Result<()> {
    let term = Bruijn::new(&args().collect::<Vec<_>>()[1])?;
    let input: Bruijn = io::stdin().try_into()?;
    let output: Vec<u8> = Bruijn::app(term, input).reduce().try_into()?;
    std::io::stdout().write_all(&output)?;
    Ok(())
}
