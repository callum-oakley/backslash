use std::{
    fmt::{self, Display},
    iter::Peekable,
};

use anyhow::{anyhow, bail, Error, Result};
use lazy_static::lazy_static;
use thiserror::Error;

use crate::{bytes, int, term::Term};

#[derive(Error, Debug)]
#[error("{source}")]
pub struct OffsetError {
    pub offset: usize,
    pub source: Error,
}

impl OffsetError {
    pub fn new<E: Into<Error>>(offset: usize, source: E) -> Self {
        Self {
            offset,
            source: source.into(),
        }
    }
}

/// A term with named variables and some sugar yet to be encoded as pure lambda calculus terms.
#[derive(Clone, Debug)]
pub enum Sugar<'a> {
    Var(&'a str, usize),
    Abs(&'a str, Box<Self>, usize),
    App(Box<Self>, Box<Self>, usize),
    Int(i64, usize),
    String(String, usize),
    Test(Box<Self>, Box<Self>, usize),
}

impl<'a> Sugar<'a> {
    pub fn parse(input: &'a str) -> Result<Self> {
        let mut tokens = Tokens::new(input).peekable();
        let term = parse_term(&mut tokens)?;
        if !is_eof(&mut tokens) {
            let token = peek(&mut tokens)?;
            bail!(unexpected_token(token));
        }
        Ok(term)
    }

    pub fn desugar(&self) -> Result<Term> {
        fn with_scope<'a>(scope: &mut Vec<&'a str>, term: &'a Sugar) -> Result<Term> {
            match term {
                Sugar::Var(x, offset) => {
                    if let Some((i, _)) = scope.iter().rev().enumerate().find(|&(_, v)| x == v) {
                        Ok(Term::Var(i))
                    } else {
                        bail!(OffsetError::new(*offset, anyhow!("unbound variable {x}"),));
                    }
                }
                Sugar::Abs(arg, body, _) => {
                    scope.push(arg);
                    let res = Term::abs(with_scope(scope, body)?);
                    scope.pop();
                    Ok(res)
                }
                Sugar::App(s, t, _) => Ok(Term::app(with_scope(scope, s)?, with_scope(scope, t)?)),
                Sugar::Int(n, _) => Ok(int::encode(*n)),
                Sugar::String(s, _) => Ok(bytes::encode(s.as_bytes())),
                Sugar::Test(_, term, _) => Ok(with_scope(scope, term)?),
            }
        }

        with_scope(&mut Vec::new(), self)
    }

    fn offset(&self) -> usize {
        match self {
            Sugar::Var(_, offset)
            | Sugar::Abs(_, _, offset)
            | Sugar::App(_, _, offset)
            | Sugar::Int(_, offset)
            | Sugar::String(_, offset)
            | Sugar::Test(_, _, offset) => *offset,
        }
    }
}

fn abs<'a>(arg: &'a str, body: Sugar<'a>, offset: usize) -> Sugar<'a> {
    Sugar::Abs(arg, Box::new(body), offset)
}

fn app<'a>(s: Sugar<'a>, t: Sugar<'a>, offset: usize) -> Sugar<'a> {
    Sugar::App(Box::new(s), Box::new(t), offset)
}

fn test<'a>(t: Sugar<'a>, term: Sugar<'a>, offset: usize) -> Sugar<'a> {
    Sugar::Test(Box::new(t), Box::new(term), offset)
}

fn parse_term<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    let offset = peek(tokens)?.offset();
    let mut terms = Vec::new();
    while !is_eof(tokens) {
        let token = peek(tokens)?;
        terms.push(match token {
            Token::Punctuation(Punctuation::Backslash, _) => parse_abs(tokens),
            Token::Punctuation(Punctuation::LParen, _) => parse_bracketed(tokens),
            Token::Punctuation(Punctuation::LSquare, _) => parse_list(tokens),
            Token::Punctuation(Punctuation::Exclamation, _) => parse_test(tokens),
            Token::Int(..) => parse_int(tokens),
            Token::String(..) => parse_string(tokens),
            Token::Identifier(..) => parse_var(tokens),
            Token::Punctuation(
                Punctuation::Dot
                | Punctuation::RParen
                | Punctuation::RSquare
                | Punctuation::Eq
                | Punctuation::Semi
                | Punctuation::Comma,
                _,
            ) => break,
        }?);
    }

    match terms.len() {
        0 => bail!(OffsetError::new(offset, anyhow!("empty term"))),
        1 => Ok(terms.remove(0)),
        _ => {
            let term = terms.remove(0);
            Ok(terms.into_iter().fold(term, |s, t| {
                let offset = s.offset();
                app(s, t, offset)
            }))
        }
    }
}

fn parse_bracketed<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    consume(Punctuation::LParen, tokens)?;
    let res = parse_term(tokens);
    consume(Punctuation::RParen, tokens)?;
    res
}

fn parse_abs<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    let offset = peek(tokens)?.offset();
    consume(Punctuation::Backslash, tokens)?;
    let mut args = Vec::new();
    while !matches!(peek(tokens)?, Token::Punctuation(Punctuation::Dot, _)) {
        args.push(parse_identifier(tokens)?.0);
    }
    consume(Punctuation::Dot, tokens)?;
    let body = parse_term(tokens)?;

    Ok(args
        .iter()
        .rev()
        .fold(body, |body, arg| abs(arg, body, offset)))
}

fn parse_var<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    let (v, offset) = parse_identifier(tokens)?;
    if !is_eof(tokens) && matches!(peek(tokens)?, Token::Punctuation(Punctuation::Eq, _)) {
        parse_let(v, offset, tokens)
    } else {
        Ok(Sugar::Var(v, offset))
    }
}

fn parse_let<'a>(
    v: &'a str,
    offset: usize,
    tokens: &mut Peekable<Tokens<'a>>,
) -> Result<Sugar<'a>> {
    lazy_static! {
        static ref FIX: Sugar<'static> = Sugar::parse(r"\f.(\x.f (x x)) (\x.f (x x))").unwrap();
    }

    consume(Punctuation::Eq, tokens)?;
    let s = parse_term(tokens)?;
    consume(Punctuation::Semi, tokens)?;
    let t = parse_term(tokens)?;

    Ok(app(
        abs(v, t, offset),
        app(FIX.clone(), abs(v, s, offset), offset),
        offset,
    ))
}

fn parse_test<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    let offset = peek(tokens)?.offset();
    consume(Punctuation::Exclamation, tokens)?;
    let t = parse_term(tokens)?;
    consume(Punctuation::Semi, tokens)?;
    let term = parse_term(tokens)?;
    Ok(test(t, term, offset))
}

fn parse_int<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    let token = next(tokens)?;
    match token {
        Token::Int(n, offset) => Ok(Sugar::Int(n, offset)),
        _ => bail!(unexpected_token(&token)),
    }
}

fn parse_string<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    let token = next(tokens)?;
    match token {
        Token::String(s, offset) => Ok(Sugar::String(s, offset)),
        _ => bail!(unexpected_token(&token)),
    }
}

fn parse_list<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    let offset = peek(tokens)?.offset();
    consume(Punctuation::LSquare, tokens)?;

    let mut terms = Vec::new();
    loop {
        terms.push(parse_term(tokens)?);

        let token = next(tokens)?;
        match token {
            Token::Punctuation(Punctuation::Comma, _) => (),
            Token::Punctuation(Punctuation::RSquare, _) => break,
            _ => bail!(unexpected_token(&token)),
        }
    }

    Ok(encode_list(terms.into_iter(), offset))
}

fn parse_identifier<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<(&'a str, usize)> {
    let token = next(tokens)?;
    match token {
        Token::Identifier(i, offset) => Ok((i, offset)),
        _ => bail!(unexpected_token(&token)),
    }
}

fn encode_list<'a>(mut terms: impl Iterator<Item = Sugar<'a>>, offset: usize) -> Sugar<'a> {
    lazy_static! {
        static ref NIL: Sugar<'static> = Sugar::parse(r"\ifnil ifcons.ifnil").unwrap();
        static ref CONS: Sugar<'static> = Sugar::parse(r"\a b ifnil ifcons.ifcons a b").unwrap();
    }
    if let Some(term) = terms.next() {
        app(
            app(CONS.clone(), term, offset),
            encode_list(terms, offset),
            offset,
        )
    } else {
        NIL.clone()
    }
}

fn is_eof(tokens: &mut Peekable<Tokens>) -> bool {
    tokens.peek().is_none()
}

fn peek<'a, 'b>(tokens: &'b mut Peekable<Tokens<'a>>) -> Result<&'b Token<'a>> {
    match tokens.peek() {
        Some(token) => token
            .as_ref()
            // We need to return an owned error but peeking only gives us a reference so flatten the
            // error in to a new anyhow::Error while taking care to properly forward OffsetError.
            .map_err(|err| match err.downcast_ref::<OffsetError>() {
                Some(err) => anyhow!(OffsetError::new(err.offset, anyhow!("{}", err.source))),
                None => anyhow!("{err}"),
            }),
        None => bail!("EOF"),
    }
}

fn next<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Token<'a>> {
    match tokens.next() {
        Some(token) => token,
        None => bail!("EOF"),
    }
}

fn consume(expected: Punctuation, tokens: &mut Peekable<Tokens>) -> Result<()> {
    let token = next(tokens)?;
    match token {
        Token::Punctuation(p, _) if p == expected => Ok(()),
        _ => bail!(unexpected_token(&token)),
    }
}

fn unexpected_token(token: &Token) -> OffsetError {
    OffsetError::new(token.offset(), anyhow!("unexpected '{token}'"))
}

#[derive(PartialEq, Clone, Copy)]
enum Punctuation {
    Backslash,
    Dot,
    LParen,
    RParen,
    LSquare,
    RSquare,
    Eq,
    Semi,
    Comma,
    Exclamation,
}

enum Token<'a> {
    Punctuation(Punctuation, usize),
    Int(i64, usize),
    String(String, usize),
    Identifier(&'a str, usize),
}

impl<'a> Token<'a> {
    fn offset(&self) -> usize {
        match self {
            Token::Punctuation(_, offset)
            | Token::Int(_, offset)
            | Token::String(_, offset)
            | Token::Identifier(_, offset) => *offset,
        }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Punctuation(Punctuation::Backslash, _) => write!(f, "\\"),
            Token::Punctuation(Punctuation::Dot, _) => write!(f, "."),
            Token::Punctuation(Punctuation::LParen, _) => write!(f, "("),
            Token::Punctuation(Punctuation::RParen, _) => write!(f, ")"),
            Token::Punctuation(Punctuation::LSquare, _) => write!(f, "["),
            Token::Punctuation(Punctuation::RSquare, _) => write!(f, "]"),
            Token::Punctuation(Punctuation::Eq, _) => write!(f, "="),
            Token::Punctuation(Punctuation::Semi, _) => write!(f, ";"),
            Token::Punctuation(Punctuation::Comma, _) => write!(f, ","),
            Token::Punctuation(Punctuation::Exclamation, _) => write!(f, "!"),
            Token::Int(n, _) => write!(f, "{n}"),
            Token::String(s, _) => write!(f, "{s}"),
            Token::Identifier(i, _) => write!(f, "{i}"),
        }
    }
}

struct Tokens<'a> {
    input: &'a str,
    offset: usize,
}

impl<'a> Tokens<'a> {
    fn new(input: &'a str) -> Self {
        Tokens { input, offset: 0 }
    }

    fn punctuation(&mut self, c: u8, punctuation: Punctuation) -> Result<Token<'a>> {
        let offset = self.offset;
        self.consume(c)?;
        Ok(Token::Punctuation(punctuation, offset))
    }

    fn char(&mut self) -> Result<Token<'a>> {
        let offset = self.offset;
        self.consume(b'\'')?;
        let c = self.char_sequence()?;
        self.consume(b'\'')?;
        Ok(Token::Int(c.into(), offset))
    }

    fn string(&mut self) -> Result<Token<'a>> {
        let offset = self.offset;
        self.consume(b'"')?;
        let mut s = Vec::new();
        while self.peek()? != b'"' {
            s.push(self.char_sequence()?);
        }
        self.consume(b'"')?;
        Ok(Token::String(
            String::from_utf8(s).map_err(|err| OffsetError::new(offset, err))?,
            offset,
        ))
    }

    fn int(&mut self) -> Result<Token<'a>> {
        let offset = self.offset;
        while !self.is_eof() && !is_reserved(self.peek()?) {
            self.next()?;
        }
        Ok(Token::Int(
            self.input[offset..self.offset]
                .parse()
                .map_err(|err| OffsetError::new(offset, err))?,
            offset,
        ))
    }

    fn identifier(&mut self) -> Result<Token<'a>> {
        let offset = self.offset;
        while !self.is_eof() && !is_reserved(self.peek()?) {
            self.next()?;
        }
        Ok(Token::Identifier(&self.input[offset..self.offset], offset))
    }

    fn char_sequence(&mut self) -> Result<u8> {
        Ok(if self.peek()? == b'\\' {
            self.consume(b'\\')?;
            match self.next()? {
                b'n' => b'\n',
                b'r' => b'\r',
                b't' => b'\t',
                c => c,
            }
        } else {
            self.next()?
        })
    }

    fn consume_whitespace(&mut self) -> Result<()> {
        while !self.is_eof() {
            match self.peek()? {
                b'#' => while self.next()? != b'\n' {},
                c if c.is_ascii_whitespace() => {
                    self.next()?;
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn is_eof(&self) -> bool {
        self.input.as_bytes().get(self.offset).is_none()
    }

    fn peek(&self) -> Result<u8> {
        match self.input.as_bytes().get(self.offset).copied() {
            Some(c) => Ok(c),
            None => bail!("EOF"),
        }
    }

    fn next(&mut self) -> Result<u8> {
        let c = self.peek()?;
        self.offset += 1;
        Ok(c)
    }

    fn consume(&mut self, expected: u8) -> Result<()> {
        let offset = self.offset;
        let c = self.next()?;
        if c != expected {
            bail!(OffsetError::new(
                offset,
                anyhow!(
                    "expected {} but got {}",
                    char::from(expected),
                    char::from(c),
                ),
            ));
        };
        Ok(())
    }
}

/// Characters that can't appear in an identifier or an int.
fn is_reserved(c: u8) -> bool {
    br#"#\.()[];,='""#.contains(&c) || c.is_ascii_whitespace()
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Err(err) = self.consume_whitespace() {
            return Some(Err(err));
        }

        if self.is_eof() {
            return None;
        }

        let token = self.peek().and_then(|c| match c {
            b'\\' => self.punctuation(c, Punctuation::Backslash),
            b'.' => self.punctuation(c, Punctuation::Dot),
            b'(' => self.punctuation(c, Punctuation::LParen),
            b')' => self.punctuation(c, Punctuation::RParen),
            b'[' => self.punctuation(c, Punctuation::LSquare),
            b']' => self.punctuation(c, Punctuation::RSquare),
            b'=' => self.punctuation(c, Punctuation::Eq),
            b';' => self.punctuation(c, Punctuation::Semi),
            b',' => self.punctuation(c, Punctuation::Comma),
            b'!' => self.punctuation(c, Punctuation::Exclamation),
            b'\'' => self.char(),
            b'"' => self.string(),
            b'-' | b'0'..=b'9' => self.int(),
            _ => self.identifier(),
        });
        Some(token)
    }
}
