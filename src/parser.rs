use std::{iter::Peekable, str::FromStr};

use anyhow::{bail, ensure, Context, Result};
use lazy_static::lazy_static;
use regex::{Match, Matches, Regex};

use crate::{int, term::Term};

impl FromStr for Term {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        fn desugar<'a>(scope: &mut Vec<&'a str>, term: &'a Sugar) -> Result<Term> {
            match term {
                Sugar::Var(x) => {
                    if let Some((i, _)) = scope.iter().rev().enumerate().find(|&(_, v)| x == v) {
                        Ok(Term::Var(i))
                    } else {
                        bail!("unbound variable {x}");
                    }
                }
                Sugar::Abs(arg, body) => {
                    scope.push(arg);
                    let res = Term::abs(desugar(scope, body)?);
                    scope.pop();
                    Ok(res)
                }
                Sugar::App(s, t) => Ok(Term::app(desugar(scope, s)?, desugar(scope, t)?)),
                Sugar::Int(n) => Ok(int::encode(*n)),
            }
        }

        desugar(&mut Vec::new(), &parse(s)?)
    }
}

/// A term with named variables and some sugar yet to be encoded as pure lambda calculus terms.
#[derive(Clone)]
enum Sugar<'a> {
    Var(&'a str),
    Abs(&'a str, Box<Sugar<'a>>),
    App(Box<Sugar<'a>>, Box<Sugar<'a>>),
    Int(i64),
}

fn abs<'a>(arg: &'a str, body: Sugar<'a>) -> Sugar<'a> {
    Sugar::Abs(arg, Box::new(body))
}

fn app<'a>(s: Sugar<'a>, t: Sugar<'a>) -> Sugar<'a> {
    Sugar::App(Box::new(s), Box::new(t))
}

fn parse(input: &str) -> Result<Sugar> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"#.*|[\\\.\(\);]|[^#\\\.\(\);\s]+").unwrap();
    }

    let mut tokens = RE.find_iter(input).peekable();
    let term = parse_term(&mut tokens)?;
    if !is_eof(&mut tokens) {
        bail!("unexpected '{}'", peek(&mut tokens)?);
    }
    Ok(term)
}

fn parse_term<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Sugar<'a>> {
    while peek(tokens)?.starts_with('#') {
        next(tokens)?;
    }

    let mut terms = Vec::new();
    while !is_eof(tokens) {
        let token = peek(tokens)?;
        terms.push(match token {
            ")" | ";" => {
                break;
            }
            "." | "=" => bail!("unexpected '{token}'"),
            "(" => parse_bracketed(tokens),
            r"\" => parse_abs(tokens),
            "let" => parse_let(tokens),
            token if is_int(token) => parse_int(tokens),
            _ => parse_identifier(tokens).map(Sugar::Var),
        }?);
    }

    match terms.len() {
        0 => bail!("empty term"),
        1 => Ok(terms.remove(0)),
        _ => {
            let term = terms.remove(0);
            Ok(terms.into_iter().fold(term, app))
        }
    }
}

fn parse_bracketed<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Sugar<'a>> {
    consume("(", tokens)?;
    let res = parse_term(tokens);
    consume(")", tokens)?;
    res
}

fn parse_abs<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Sugar<'a>> {
    consume(r"\", tokens)?;
    let mut args = Vec::new();
    while peek(tokens)? != "." {
        args.push(parse_identifier(tokens)?);
    }
    consume(".", tokens)?;
    let body = parse_term(tokens)?;

    Ok(args.iter().rev().fold(body, |body, arg| abs(arg, body)))
}

fn parse_let<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Sugar<'a>> {
    lazy_static! {
        static ref FIX: Sugar<'static> = parse(r"\f.(\x.f (x x)) (\x.f (x x))").unwrap();
    }

    consume("let", tokens)?;
    let arg = parse_identifier(tokens)?;
    consume("=", tokens)?;
    let s = parse_term(tokens)?;
    consume(";", tokens)?;
    let t = parse_term(tokens)?;

    Ok(app(abs(arg, t), app(FIX.clone(), abs(arg, s))))
}

fn parse_int<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Sugar<'a>> {
    next(tokens)?.parse().map(Sugar::Int).map_err(Into::into)
}

fn parse_identifier<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<&'a str> {
    let token = next(tokens)?;
    if matches!(token, r"\" | "." | "(" | ")" | "let" | "=" | ";") {
        bail!("unexpected '{token}'");
    }
    if is_int(token) {
        bail!("identifier can't be an int");
    }
    Ok(token)
}

fn is_int(s: &str) -> bool {
    matches!(s.as_bytes(), [c, ..] | [b'+' | b'-', c, ..] if c.is_ascii_digit())
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
