use std::{
    env,
    fs::File,
    io::{self, Read, Write},
};

use anyhow::{bail, Result};
use bs::run;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        bail!("Usage: bs FILES");
    }

    let mut term = String::new();
    for file in &args[1..] {
        File::open(file)?.read_to_string(&mut term)?;
        term.push('\n');
    }

    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input)?;

    let output = run(&term, &input)?;
    io::stdout().write_all(&output)?;

    Ok(())
}
