use std::{
    env,
    fs::File,
    io::{self, Read, Write},
};

use anyhow::{bail, Result};
use bs::run;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        bail!("Usage: bs FILE");
    }

    let mut term = String::new();
    File::open(&args[1])?.read_to_string(&mut term)?;

    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input)?;

    let output = run(&term, &input)?;
    io::stdout().write_all(&output)?;

    Ok(())
}
