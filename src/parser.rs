use anyhow::Result;
use nom::{
    branch::alt,
    bytes::complete::{is_not, take_while1},
    character::complete::{char, multispace1},
    combinator::{eof, map, value},
    multi::{many0, many1, many_m_n},
    sequence::{delimited, pair, terminated},
    IResult,
};

/// Represents a term as written: multiple argument abstractions, two or more terms applied in
/// sequence, let bindings (TODO).
pub enum Term<'a> {
    Var(&'a str),
    Abs(Vec<&'a str>, Box<Term<'a>>),
    App(Vec<Term<'a>>),
}

impl<'a> Term<'a> {
    pub fn new(input: &'a str) -> Result<Self> {
        Ok(terminated(term, eof)(input)
            .map_err(|err| err.to_owned())?
            .1)
    }
}

fn comment(input: &str) -> IResult<&str, ()> {
    value((), pair(char('#'), is_not("\n\r")))(input)
}

fn whitespace(input: &str) -> IResult<&str, ()> {
    value((), many0(alt((value((), multispace1), comment))))(input)
}

fn ws<'a, F, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(whitespace, inner, whitespace)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    ws(take_while1(|c: char| {
        !c.is_whitespace() && !r"\.()".contains(c)
    }))(input)
}

fn variable(input: &str) -> IResult<&str, Term> {
    map(identifier, Term::Var)(input)
}

fn abstraction(input: &str) -> IResult<&str, Term> {
    ws(map(
        pair(delimited(char('\\'), many1(identifier), char('.')), term),
        |(args, body)| Term::Abs(args, Box::new(body)),
    ))(input)
}

fn application(input: &str) -> IResult<&str, Term> {
    map(
        many_m_n(2, usize::MAX, alt((bracketed, abstraction, variable))),
        Term::App,
    )(input)
}

fn bracketed(input: &str) -> IResult<&str, Term> {
    ws(delimited(char('('), term, char(')')))(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    alt((application, bracketed, abstraction, variable))(input)
}
