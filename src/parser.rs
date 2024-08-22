use std::{iter::Peekable, str::FromStr};

use anyhow::{bail, ensure, Context, Result};
use lazy_static::lazy_static;
use regex::{Match, Matches, Regex};

use crate::{bytes, int, term::Term};

const TOKENS: &str = r#"#.*|'(\\.|[^'])'|"(\\.|[^"])*"|[\\\.\(\);]|[^#'"\\\.\(\);\s]+"#;

impl FromStr for Term {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        fn desugar<'a>(scope: &mut Vec<&'a str>, term: &'a Sugar) -> Result<Term> {
            match term {
                Sugar::Var(x) => {
                    if let Some((i, _)) = scope.iter().rev().enumerate().find(|&(_, v)| x == v) {
                        Ok(Term::Var(i))
                    } else {
                        bail!("parser: unbound variable {x}");
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
                Sugar::Bytes(bytes) => Ok(bytes::encode(bytes)),
            }
        }

        desugar(&mut Vec::new(), &parse(s)?)
    }
}

/// A term with named variables and some sugar yet to be encoded as pure lambda calculus terms.
#[derive(Clone)]
enum Sugar<'a> {
    Var(&'a str),
    Abs(&'a str, Box<Self>),
    App(Box<Self>, Box<Self>),
    Int(i64),
    Bytes(Vec<u8>),
}

fn abs<'a>(arg: &'a str, body: Sugar<'a>) -> Sugar<'a> {
    Sugar::Abs(arg, Box::new(body))
}

fn app<'a>(s: Sugar<'a>, t: Sugar<'a>) -> Sugar<'a> {
    Sugar::App(Box::new(s), Box::new(t))
}

fn parse(input: &str) -> Result<Sugar> {
    lazy_static! {
        static ref TOKENS_RE: Regex = Regex::new(TOKENS).unwrap();
    }

    let mut tokens = TOKENS_RE.find_iter(input).peekable();
    let term = parse_term(&mut tokens)?;
    if !is_eof(&mut tokens) {
        bail!("parser: unexpected '{}'", peek(&mut tokens)?);
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
            "." | "=" => bail!("parser: unexpected '{token}'"),
            "(" => parse_bracketed(tokens),
            r"\" => parse_abs(tokens),
            "let" => parse_let(tokens),
            token if is_int(token) => parse_int(tokens),
            token if is_char(token) => parse_char(tokens),
            token if is_string(token) => parse_string(tokens),
            _ => parse_identifier(tokens).map(Sugar::Var),
        }?);
    }

    match terms.len() {
        0 => bail!("parser: empty term"),
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

fn parse_char<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Sugar<'a>> {
    let token = next(tokens)?;
    match token.as_bytes() {
        b"'\\n'" => Ok(Sugar::Int(b'\n'.into())),
        b"'\\r'" => Ok(Sugar::Int(b'\r'.into())),
        b"'\\t'" => Ok(Sugar::Int(b'\t'.into())),
        [b'\'', c, b'\''] | [b'\'', b'\\', c, b'\''] => Ok(Sugar::Int((*c).into())),
        _ => bail!("parser: malformed char '{token}'"),
    }
}

fn parse_string<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<Sugar<'a>> {
    let token = next(tokens)?.as_bytes();
    assert_eq!(token[0], b'"');
    assert_eq!(token[token.len() - 1], b'"');
    let mut bytes = Vec::new();
    let mut escape = false;
    for &c in &token[1..token.len() - 1] {
        if escape {
            match c {
                b'n' => bytes.push(b'\n'),
                b'r' => bytes.push(b'\r'),
                b't' => bytes.push(b'\t'),
                c => bytes.push(c),
            }
            escape = false;
        } else if c == b'\\' {
            escape = true;
        } else {
            bytes.push(c);
        }
    }
    Ok(Sugar::Bytes(bytes))
}

fn parse_identifier<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<&'a str> {
    let token = next(tokens)?;
    if matches!(token, r"\" | "." | "(" | ")" | "let" | "=" | ";") {
        bail!("parser: unexpected '{token}'");
    }
    if is_int(token) {
        bail!("parser: identifier can't be an int");
    }
    Ok(token)
}

fn is_int(s: &str) -> bool {
    matches!(s.as_bytes(), [c, ..] | [b'+' | b'-', c, ..] if c.is_ascii_digit())
}

fn is_char(s: &str) -> bool {
    s.as_bytes()[0] == b'\''
}

fn is_string(s: &str) -> bool {
    s.as_bytes()[0] == b'"'
}

fn is_eof(tokens: &mut Peekable<Matches>) -> bool {
    tokens.peek().is_none()
}

fn peek<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<&'a str> {
    tokens.peek().context("parser: EOF").map(Match::as_str)
}

fn next<'a>(tokens: &mut Peekable<Matches<'_, 'a>>) -> Result<&'a str> {
    tokens.next().context("parser: EOF").map(|m| m.as_str())
}

fn consume(expected: &str, tokens: &mut Peekable<Matches>) -> Result<()> {
    let token = next(tokens)?;
    ensure!(token == expected, "parser: unexpected '{token}'");
    Ok(())
}
