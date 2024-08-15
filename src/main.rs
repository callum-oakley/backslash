use std::{
    env,
    io::{self, Read, Write},
};

use anyhow::{bail, Result};
use bs::run;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        bail!("Usage: bs FILE");
    }
    let term = &args[1];

    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input)?;

    let output = run(term, &input)?;
    io::stdout().write_all(&output)?;

    Ok(())
}
