use anyhow::Result;
use nom::{
    branch::alt,
    bytes::complete::{is_not, take_while1},
    character::complete::{char, multispace1},
    combinator::{eof, map, value},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

#[derive(PartialEq, Debug)]
enum Term<'a> {
    Var(&'a str),
    Abs(&'a str, Box<Term<'a>>),
    App(Box<Term<'a>>, Box<Term<'a>>),
}

impl<'a> Term<'a> {
    fn new(input: &'a str) -> Result<Self> {
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

fn abstraction_body(input: &str) -> IResult<&str, Term> {
    ws(alt((
        map(pair(identifier, abstraction_body), |(identifier, body)| {
            Term::Abs(identifier, Box::new(body))
        }),
        preceded(char('.'), term),
    )))(input)
}

fn abstraction(input: &str) -> IResult<&str, Term> {
    ws(preceded(char('\\'), abstraction_body))(input)
}

// TODO can we rewrite this in a functional style? It's kind of the odd one out...
fn application(input: &str) -> IResult<&str, Term> {
    let (input, _) = whitespace(input)?;
    let (mut input, (mut s, t)) = pair(
        alt((bracketed, abstraction, variable)),
        alt((bracketed, abstraction, variable)),
    )(input)?;
    s = Term::App(Box::new(s), Box::new(t));
    while let Ok((i, t)) = (alt((bracketed, abstraction, variable)))(input) {
        input = i;
        s = Term::App(Box::new(s), Box::new(t));
    }
    let (input, _) = whitespace(input)?;
    Ok((input, s))
}

fn bracketed(input: &str) -> IResult<&str, Term> {
    ws(delimited(char('('), term, char(')')))(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    alt((application, bracketed, abstraction, variable))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(var: &str) -> Term {
        Term::Var(var)
    }

    fn abs<'a>(var: &'a str, body: Term<'a>) -> Term<'a> {
        Term::Abs(var, Box::new(body))
    }

    fn app<'a>(s: Term<'a>, t: Term<'a>) -> Term<'a> {
        Term::App(Box::new(s), Box::new(t))
    }

    #[test]
    fn test_parse() {
        assert_eq!(Term::new("x").unwrap(), var("x"));
        assert_eq!(Term::new(r"\x.x").unwrap(), abs("x", var("x")));
        assert_eq!(Term::new("x y").unwrap(), app(var("x"), var("y")));
        assert_eq!(
            Term::new(r"\x.\y.x y").unwrap(),
            abs("x", abs("y", app(var("x"), var("y")))),
        );
        assert_eq!(
            Term::new("x y z").unwrap(),
            app(app(var("x"), var("y")), var("z")),
        );
        assert_eq!(
            Term::new(r"\g.(\x.g (x x)) (\x.g (x x))").unwrap(),
            abs(
                "g",
                app(
                    abs("x", app(var("g"), app(var("x"), var("x")))),
                    abs("x", app(var("g"), app(var("x"), var("x")))),
                ),
            ),
        );
        assert_eq!(
            Term::new(r"\x y z.x y z").unwrap(),
            abs(
                "x",
                abs("y", abs("z", app(app(var("x"), var("y")), var("z")))),
            )
        );
        assert_eq!(Term::new(r"\.x y").unwrap(), app(var("x"), var("y")));
        assert_eq!(
            Term::new(r"\_.x y").unwrap(),
            abs("_", app(var("x"), var("y")))
        );
        assert_eq!(
            Term::new("# The identity:\n\\x.x").unwrap(),
            abs("x", var("x")),
        );
    }
}
