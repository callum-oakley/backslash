use std::{
    env,
    fs::File,
    io::{self, Read, Write},
};

use anyhow::{bail, Result};
use bs::run;

fn main() -> Result<()> {
    // TODO flag to run tests
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        bail!("Usage: bs FILES");
    }

    let mut file_contents = Vec::new();
    for path in &args[1..] {
        let mut file_content = String::new();
        File::open(path)?.read_to_string(&mut file_content)?;
        file_contents.push(file_content);
    }

    let mut files = Vec::new();
    for (i, path) in args[1..].iter().enumerate() {
        files.push((path.as_str(), file_contents[i].as_str()));
    }

    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input)?;

    let output = run(files, &input)?;
    io::stdout().write_all(&output)?;

    Ok(())
}
