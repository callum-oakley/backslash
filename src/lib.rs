#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]

use anyhow::{anyhow, Error, Result};
use sugar::{OffsetError, Sugar};
use term::Term;

mod bytes;
mod int;
mod list;
mod sugar;
mod term;
mod test;

static STD: &str = include_str!("std.bs");

pub fn run(mut files: Vec<(&str, &str)>, input: &[u8]) -> Result<Vec<u8>> {
    files.insert(0, ("src/std.bs", STD));
    let mut term = String::new();
    for (_, file) in &files {
        term.push_str(file);
    }
    Sugar::parse(&term)
        .and_then(|sugar| Sugar::desugar(&sugar))
        .and_then(|term| bytes::decode(Term::app(term, bytes::encode(input)).reduce()))
        .map_err(|err| offset_to_file_line_col(&files, err))
}

pub fn run_tests(mut files: Vec<(&str, &str)>) -> Result<()> {
    files.insert(0, ("src/std.bs", STD));
    let mut term = String::new();
    for (_, file) in &files {
        term.push_str(file);
    }
    Sugar::parse(&term)
        .and_then(test::run)
        .map_err(|err| offset_to_file_line_col(&files, err))
}

fn offset_to_file_line_col(files: &[(&str, &str)], err: Error) -> Error {
    match err.downcast_ref::<OffsetError>() {
        Some(err) => {
            let mut offset = err.offset;

            let mut file = 0;
            while offset >= files[file].1.len() {
                offset -= files[file].1.len();
                file += 1;
            }

            let mut line = 1;
            let mut col = 1;
            for i in 0..offset {
                if files[file].1.as_bytes()[i] == b'\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            }

            anyhow!("{} at {}:{}:{}", err.source, files[file].0, line, col)
        }
        None => err,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn std() -> Result<()> {
        run_tests(vec![("TEST", "id")])
    }
}
