use std::iter::Peekable;

use anyhow::{bail, ensure, Context, Result};
use regex::{Match, Matches, Regex};

pub enum Term<'a> {
    Var(&'a str),
    Abs(&'a str, Box<Term<'a>>),
    App(Box<Term<'a>>, Box<Term<'a>>),
    Int(i64),
}

impl<'a> Term<'a> {
    pub fn new(input: &'a str) -> Result<Self> {
        let re = Regex::new(r"#.*|[\\\.\(\)]|[^#\\\.\(\)\s]+").unwrap();
        let mut tokens = re.find_iter(input).peekable();
        let term = parse_term(&mut tokens)?;
        if !is_eof(&mut tokens) {
            bail!("unexpected '{}'", peek(&mut tokens)?);
        }
        Ok(term)
    }

    #[must_use]
    pub fn abs(arg: &'a str, body: Self) -> Self {
        Term::Abs(arg, Box::new(body))
    }

    #[must_use]
    pub fn app(s: Self, t: Self) -> Self {
        Term::App(Box::new(s), Box::new(t))
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
            _ => parse_identifier(tokens).map(|id| id.parse().map_or(Term::Var(id), Term::Int)),
        }?);
    }

    match terms.len() {
        0 => bail!("empty term"),
        1 => Ok(terms.remove(0)),
        _ => {
            let term = terms.remove(0);
            Ok(terms.into_iter().fold(term, Term::app))
        }
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
    Ok(args
        .iter()
        .rev()
        .fold(parse_term(tokens)?, |body, arg| Term::abs(arg, body)))
}

fn parse_let<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Term<'a>> {
    consume("let", tokens)?;
    let arg = parse_identifier(tokens)?;
    consume("=", tokens)?;
    let s = parse_term(tokens)?;
    consume("in", tokens)?;
    let t = parse_term(tokens)?;
    Ok(Term::app(
        Term::abs(arg, t),
        Term::app(Term::new(r"\f.(\x.f(x x))(\x.f(x x))")?, Term::abs(arg, s)),
    ))
}
