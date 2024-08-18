use std::iter::Peekable;

use anyhow::{bail, ensure, Context, Result};
use regex::{Match, Matches, Regex};

/// Represents a term as written: multiple argument abstractions, two or more terms applied in
/// sequence, let bindings.
pub enum Term<'a> {
    Var(&'a str),
    Abs(Vec<&'a str>, Box<Term<'a>>),
    App(Vec<Term<'a>>),
    Let(&'a str, Box<Term<'a>>, Box<Term<'a>>),
}

impl<'a> Term<'a> {
    pub fn new(input: &'a str) -> Result<Self> {
        let re = Regex::new(r"#.*|\\|\.|\(|\)|[^\\\.\(\)\s]+").unwrap();
        let mut tokens = re.find_iter(input).peekable();
        let term = parse_term(&mut tokens)?;
        if !is_eof(&mut tokens) {
            bail!("unexpected '{}'", peek(&mut tokens)?);
        }
        Ok(term)
    }
}

fn is_eof(tokens: &mut Peekable<Matches>) -> bool {
    tokens.peek().is_none()
}

fn peek<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<&'a str> {
    tokens.peek().context("EOF").map(Match::as_str)
}

fn next<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<&'a str> {
    tokens.next().context("EOF").map(|m| m.as_str())
}

fn consume(expected: &str, tokens: &mut Peekable<Matches>) -> Result<()> {
    ensure!(tokens.next().context("EOF")?.as_str() == expected);
    Ok(())
}

fn parse_identifier<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<&'a str> {
    let token = next(tokens)?;
    match token {
        r"\" | "." | "(" | ")" | "let" | "=" | "in" => bail!("unexpected '{token}'"),
        _ => Ok(token),
    }
}

fn parse_term<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Term<'a>> {
    while peek(tokens)?.starts_with('#') {
        next(tokens)?;
    }

    let mut terms = Vec::new();
    while !is_eof(tokens) {
        let token = peek(tokens)?;
        terms.push(match token {
            ")" | "in" => {
                break;
            }
            "." | "=" => bail!("unexpected '{token}'"),
            "(" => parse_bracketed(tokens),
            r"\" => parse_abs(tokens),
            "let" => parse_let(tokens),
            _ => parse_identifier(tokens).map(Term::Var),
        }?);
    }

    match terms.len() {
        0 => bail!("empty term"),
        1 => Ok(terms.swap_remove(0)),
        _ => Ok(Term::App(terms)),
    }
}

fn parse_bracketed<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Term<'a>> {
    consume("(", tokens)?;
    let res = parse_term(tokens);
    consume(")", tokens)?;
    res
}

fn parse_abs<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Term<'a>> {
    consume(r"\", tokens)?;
    let mut args = Vec::new();
    while peek(tokens)? != "." {
        args.push(parse_identifier(tokens)?);
    }
    consume(".", tokens)?;
    let body = parse_term(tokens)?;
    Ok(Term::Abs(args, Box::new(body)))
}

fn parse_let<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Term<'a>> {
    consume("let", tokens)?;
    let arg = parse_identifier(tokens)?;
    consume("=", tokens)?;
    let s = parse_term(tokens)?;
    consume("in", tokens)?;
    let t = parse_term(tokens)?;
    Ok(Term::Let(arg, Box::new(s), Box::new(t)))
}
