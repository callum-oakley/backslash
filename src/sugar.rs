use std::{
    fmt::{self, Display},
    iter::Peekable,
};

use anyhow::{anyhow, bail, ensure, Context, Result};
use lazy_static::lazy_static;

use crate::{bytes, int, term::Term};

/// A term with named variables and some sugar yet to be encoded as pure lambda calculus terms.
#[derive(Clone)]
pub enum Sugar<'a> {
    Var(&'a str),
    Abs(&'a str, Box<Self>),
    App(Box<Self>, Box<Self>),
    Int(i64),
    String(String),
    Test(Box<Self>, Box<Self>, Box<Self>),
}

impl<'a> Sugar<'a> {
    pub fn parse(input: &'a str) -> Result<Self> {
        let mut tokens = Tokens::new(input).peekable();
        let term = parse_term(&mut tokens)?;
        if !is_eof(&mut tokens) {
            bail!("parser: unexpected '{}'", peek(&mut tokens)?);
        }
        Ok(term)
    }

    pub fn desugar(&self) -> Result<Term> {
        fn with_scope<'a>(scope: &mut Vec<&'a str>, term: &'a Sugar) -> Result<Term> {
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
                    let res = Term::abs(with_scope(scope, body)?);
                    scope.pop();
                    Ok(res)
                }
                Sugar::App(s, t) => Ok(Term::app(with_scope(scope, s)?, with_scope(scope, t)?)),
                Sugar::Int(n) => Ok(int::encode(*n)),
                Sugar::String(s) => Ok(bytes::encode(s.as_bytes())),
                Sugar::Test(_, _, term) => Ok(with_scope(scope, term)?),
            }
        }

        with_scope(&mut Vec::new(), self)
    }
}

fn abs<'a>(arg: &'a str, body: Sugar<'a>) -> Sugar<'a> {
    Sugar::Abs(arg, Box::new(body))
}

fn app<'a>(s: Sugar<'a>, t: Sugar<'a>) -> Sugar<'a> {
    Sugar::App(Box::new(s), Box::new(t))
}

fn test<'a>(lhs: Sugar<'a>, rhs: Sugar<'a>, term: Sugar<'a>) -> Sugar<'a> {
    Sugar::Test(Box::new(lhs), Box::new(rhs), Box::new(term))
}

fn parse_term<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    let mut terms = Vec::new();
    while !is_eof(tokens) {
        let token = peek(tokens)?;
        terms.push(match token {
            Token::Backslash => parse_abs(tokens),
            Token::LParen => parse_bracketed(tokens),
            Token::LSquare => parse_list(tokens),
            Token::Let => parse_let(tokens),
            Token::Exclamation => parse_test(tokens),
            Token::Int(_) => parse_int(tokens),
            Token::String(_) => parse_string(tokens),
            Token::Identifier(_) => parse_identifier(tokens).map(Sugar::Var),
            _ => break,
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

fn parse_bracketed<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    consume(&Token::LParen, tokens)?;
    let res = parse_term(tokens);
    consume(&Token::RParen, tokens)?;
    res
}

fn parse_abs<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    consume(&Token::Backslash, tokens)?;
    let mut args = Vec::new();
    while peek(tokens)? != &Token::Dot {
        args.push(parse_identifier(tokens)?);
    }
    consume(&Token::Dot, tokens)?;
    let body = parse_term(tokens)?;

    Ok(args.iter().rev().fold(body, |body, arg| abs(arg, body)))
}

fn parse_let<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    lazy_static! {
        static ref FIX: Sugar<'static> = Sugar::parse(r"\f.(\x.f (x x)) (\x.f (x x))").unwrap();
    }

    consume(&Token::Let, tokens)?;
    let arg = parse_identifier(tokens)?;
    consume(&Token::Eq, tokens)?;
    let s = parse_term(tokens)?;
    consume(&Token::Semi, tokens)?;
    let t = parse_term(tokens)?;

    Ok(app(abs(arg, t), app(FIX.clone(), abs(arg, s))))
}

fn parse_test<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    consume(&Token::Exclamation, tokens)?;
    let lhs = parse_term(tokens)?;
    consume(&Token::Eq, tokens)?;
    let rhs = parse_term(tokens)?;
    consume(&Token::Semi, tokens)?;
    let term = parse_term(tokens)?;
    Ok(test(lhs, rhs, term))
}

fn parse_int<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    match next(tokens)? {
        Token::Int(n) => Ok(Sugar::Int(n)),
        token => bail!("parser: unexpected '{token}'"),
    }
}

fn parse_string<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    match next(tokens)? {
        Token::String(s) => Ok(Sugar::String(s)),
        token => bail!("parser: unexpected '{token}'"),
    }
}

fn parse_list<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Sugar<'a>> {
    consume(&Token::LSquare, tokens)?;

    let mut terms = Vec::new();
    loop {
        terms.push(parse_term(tokens)?);

        match next(tokens)? {
            Token::Comma => (),
            Token::RSquare => break,
            token => bail!("unexpected '{token}'"),
        }
    }

    Ok(encode_list(terms.into_iter()))
}

fn parse_identifier<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<&'a str> {
    match next(tokens)? {
        Token::Identifier(i) => Ok(i),
        token => bail!("parser: unexpected '{token}'"),
    }
}

fn encode_list<'a>(mut terms: impl Iterator<Item = Sugar<'a>>) -> Sugar<'a> {
    lazy_static! {
        static ref NIL: Sugar<'static> = Sugar::parse(r"\ifnil ifcons.ifnil").unwrap();
        static ref CONS: Sugar<'static> = Sugar::parse(r"\a b ifnil ifcons.ifcons a b").unwrap();
    }
    if let Some(term) = terms.next() {
        app(app(CONS.clone(), term), encode_list(terms))
    } else {
        NIL.clone()
    }
}

fn is_eof(tokens: &mut Peekable<Tokens>) -> bool {
    tokens.peek().is_none()
}

fn peek<'a, 'b>(tokens: &'b mut Peekable<Tokens<'a>>) -> Result<&'b Token<'a>> {
    tokens
        .peek()
        .context("parser: EOF")
        .and_then(|r| r.as_ref().map_err(|err| anyhow!("{err}")))
}

fn next<'a>(tokens: &mut Peekable<Tokens<'a>>) -> Result<Token<'a>> {
    tokens.next().context("parser: EOF")?
}

fn consume(expected: &Token, tokens: &mut Peekable<Tokens>) -> Result<()> {
    let token = next(tokens)?;
    ensure!(&token == expected, "parser: unexpected '{token}'");
    Ok(())
}

#[derive(PartialEq)]
enum Token<'a> {
    Backslash,
    Dot,
    LParen,
    RParen,
    LSquare,
    RSquare,
    Let,
    Eq,
    Semi,
    Comma,
    Exclamation,
    Int(i64),
    String(String),
    Identifier(&'a str),
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Backslash => write!(f, "\\"),
            Token::Dot => write!(f, "."),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LSquare => write!(f, "["),
            Token::RSquare => write!(f, "]"),
            Token::Let => write!(f, "let"),
            Token::Eq => write!(f, "="),
            Token::Semi => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Exclamation => write!(f, "!"),
            Token::Int(n) => write!(f, "{n}"),
            Token::String(s) => write!(f, "{s}"),
            Token::Identifier(i) => write!(f, "{i}"),
        }
    }
}

struct Tokens<'a> {
    input: &'a str,
    i: usize,
}

impl<'a> Tokens<'a> {
    fn new(input: &'a str) -> Self {
        Tokens { input, i: 0 }
    }

    fn punctuation(&mut self, c: u8, token: Token<'a>) -> Result<Token<'a>> {
        self.consume(c)?;
        Ok(token)
    }

    fn char(&mut self) -> Result<Token<'a>> {
        self.consume(b'\'')?;
        let c = self.char_sequence()?;
        self.consume(b'\'')?;
        Ok(Token::Int(c.into()))
    }

    fn string(&mut self) -> Result<Token<'a>> {
        self.consume(b'"')?;
        let mut s = Vec::new();
        while self.peek()? != b'"' {
            s.push(self.char_sequence()?);
        }
        self.consume(b'"')?;
        Ok(Token::String(String::from_utf8(s)?))
    }

    fn int(&mut self) -> Result<Token<'a>> {
        let start = self.i;
        while !self.is_eof() && !is_reserved(self.peek()?) {
            self.next()?;
        }
        Ok(Token::Int(self.input[start..self.i].parse()?))
    }

    fn identifier(&mut self) -> Result<Token<'a>> {
        let start = self.i;
        while !self.is_eof() && !is_reserved(self.peek()?) {
            self.next()?;
        }
        let s = &self.input[start..self.i];
        if s == "let" {
            Ok(Token::Let)
        } else {
            Ok(Token::Identifier(s))
        }
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
        self.input.as_bytes().get(self.i).is_none()
    }

    fn peek(&self) -> Result<u8> {
        self.input
            .as_bytes()
            .get(self.i)
            .copied()
            .context("parser: EOF")
    }

    fn next(&mut self) -> Result<u8> {
        let c = self.peek()?;
        self.i += 1;
        Ok(c)
    }

    fn consume(&mut self, expected: u8) -> Result<()> {
        let c = self.next()?;
        ensure!(c == expected, "parser: expected {expected} but got {c}");
        Ok(())
    }
}

/// Characters that can't appear in an identifier or an int.
fn is_reserved(c: u8) -> bool {
    br#"#\.()[];,'""#.contains(&c) || c.is_ascii_whitespace()
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Result<Token<'a>>> {
        if let Err(err) = self.consume_whitespace() {
            return Some(Err(err));
        }

        if self.is_eof() {
            return None;
        }

        Some(self.peek().and_then(|c| match c {
            b'\\' => self.punctuation(c, Token::Backslash),
            b'.' => self.punctuation(c, Token::Dot),
            b'=' => self.punctuation(c, Token::Eq),
            b'(' => self.punctuation(c, Token::LParen),
            b')' => self.punctuation(c, Token::RParen),
            b'[' => self.punctuation(c, Token::LSquare),
            b']' => self.punctuation(c, Token::RSquare),
            b';' => self.punctuation(c, Token::Semi),
            b',' => self.punctuation(c, Token::Comma),
            b'!' => self.punctuation(c, Token::Exclamation),
            b'\'' => self.char(),
            b'"' => self.string(),
            b'-' | b'+' | b'0'..=b'9' => self.int(),
            _ => self.identifier(),
        }))
    }
}
