use std::env::args;

use backslash::Bruijn;

fn main() -> anyhow::Result<()> {
    let mut term: Bruijn = args().collect::<Vec<_>>()[1].parse()?;
    term.reduce();
    println!("{term}");
    Ok(())
}
