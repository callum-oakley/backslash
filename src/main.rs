use std::env::args;

use backslash::Bruijn;

fn main() -> anyhow::Result<()> {
    println!("{}", Bruijn::new(&args().collect::<Vec<_>>()[1])?.reduce());
    Ok(())
}
