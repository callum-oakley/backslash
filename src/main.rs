use std::{
    fs::File,
    io::{self, Read, Write},
    process,
};

use anyhow::Result;
use clap::Parser;

#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Source files to run
    #[arg(required = true)]
    files: Vec<String>,

    /// Run tests
    #[arg(short, long)]
    test: bool,
}

fn run() -> Result<()> {
    let args = Args::parse();

    let mut file_contents = Vec::new();
    for path in &args.files {
        let mut file_content = String::new();
        File::open(path)?.read_to_string(&mut file_content)?;
        file_contents.push(file_content);
    }

    let mut files = Vec::new();
    for (i, path) in args.files.iter().enumerate() {
        files.push((path.as_str(), file_contents[i].as_str()));
    }

    if args.test {
        let test_count = gali::run_tests(files)?;
        eprintln!("ran {test_count} tests");
        Ok(())
    } else {
        let mut input = Vec::new();
        io::stdin().read_to_end(&mut input)?;

        let output = gali::run(files, &input)?;
        io::stdout().write_all(&output)?;

        Ok(())
    }
}

fn main() {
    if let Err(err) = run() {
        let style = anstyle::Style::new()
            .bold()
            .fg_color(Some(anstyle::AnsiColor::Red.into()));
        eprintln!("{style}error:{style:#} {err:#}");
        process::exit(1);
    }
}
