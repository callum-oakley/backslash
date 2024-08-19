use std::mem;

use anyhow::{bail, Result};

use crate::{int, parser};

/// A lambda calculus term with de Bruijn indices (starting at 0).
#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Term {
    pub fn new(s: &str) -> Result<Self> {
        Intermediate::new(s).and_then(|term| (&term).try_into())
    }

    #[must_use]
    pub fn abs(body: Self) -> Self {
        Term::Abs(Box::new(body))
    }

    #[must_use]
    pub fn app(s: Self, t: Self) -> Self {
        Term::App(Box::new(s), Box::new(t))
    }

    pub fn try_unabs(self) -> Result<Self> {
        if let Self::Abs(body) = self {
            Ok(*body)
        } else {
            bail!("not an abstraction");
        }
    }

    pub fn try_unapp(self) -> Result<(Self, Self)> {
        if let Self::App(s, t) = self {
            Ok((*s, *t))
        } else {
            bail!("not an application");
        }
    }

    #[must_use]
    pub fn reduce(mut self) -> Self {
        fn substitute<'a>(
            term: &'a mut Term,
            // Exactly one of param or param_ref will be Some.
            param: &mut Option<Term>,
            param_ref: &mut Option<&'a Term>,
            depth: usize,
        ) {
            match term {
                Term::Var(x) => {
                    if *x == depth {
                        // If this is the first substitution, we can take out of param to avoid
                        // cloning, otherwise we'll have to clone from param_ref.
                        if param.is_some() {
                            *term = param.take().unwrap();
                            *param_ref = Some(term);
                        } else {
                            *term = param_ref.unwrap().clone();
                        }
                    }
                }
                Term::Abs(body) => {
                    substitute(body.as_mut(), param, param_ref, depth + 1);
                }
                Term::App(s, t) => {
                    substitute(s, param, param_ref, depth);
                    substitute(t.as_mut(), param, param_ref, depth);
                }
            }
        }

        fn reduce_once(term: &mut Term) -> bool {
            match term {
                Term::Var(_) => false,
                Term::Abs(body) => reduce_once(body.as_mut()),
                Term::App(s, t) => {
                    if let Term::Abs(body) = s.as_mut() {
                        // We "take" parts of the term below by replacing them with Bruijn::Var(0)
                        // which will be dropped immediately.
                        substitute(
                            body.as_mut(),
                            &mut Some(mem::replace(t, Term::Var(0))),
                            &mut None,
                            0,
                        );
                        *term = mem::replace(body, Term::Var(0));
                        true
                    } else {
                        reduce_once(s) || reduce_once(t)
                    }
                }
            }
        }

        while reduce_once(&mut self) {}

        self
    }
}

impl<'a> TryFrom<&Intermediate<'a>> for Term {
    type Error = anyhow::Error;

    fn try_from(term: &Intermediate<'a>) -> Result<Self> {
        fn try_from_with_scope<'a>(
            scope: &mut Vec<&'a str>,
            term: &'a Intermediate,
        ) -> Result<Term> {
            match term {
                Intermediate::Var(x) => {
                    if let Some((i, _)) = scope.iter().rev().enumerate().find(|&(_, v)| x == v) {
                        Ok(Term::Var(i))
                    } else {
                        bail!("unbound variable {x}");
                    }
                }
                Intermediate::Abs(arg, body) => {
                    scope.push(arg);
                    let res = Term::abs(try_from_with_scope(scope, body)?);
                    scope.pop();
                    Ok(res)
                }
                Intermediate::App(s, t) => Ok(Term::app(
                    try_from_with_scope(scope, s)?,
                    try_from_with_scope(scope, t)?,
                )),
                Intermediate::Int(n) => Ok(int::encode(*n)),
            }
        }

        try_from_with_scope(&mut Vec::new(), term)
    }
}

// TODO can we have parser::Term look more like this, and skip this intermediate representation?
#[derive(PartialEq, Debug)]
enum Intermediate<'a> {
    Var(&'a str),
    Abs(&'a str, Box<Intermediate<'a>>),
    App(Box<Intermediate<'a>>, Box<Intermediate<'a>>),
    Int(i64),
}

impl<'a> Intermediate<'a> {
    fn new(input: &'a str) -> Result<Self> {
        parser::Term::new(input).map(|term| (&term).into())
    }

    #[must_use]
    pub fn abs(arg: &'a str, body: Self) -> Self {
        Intermediate::Abs(arg, Box::new(body))
    }

    #[must_use]
    pub fn app(s: Self, t: Self) -> Self {
        Intermediate::App(Box::new(s), Box::new(t))
    }
}

impl<'a> From<&parser::Term<'a>> for Intermediate<'a> {
    fn from(term: &parser::Term<'a>) -> Self {
        match term {
            parser::Term::Var(x) => Intermediate::Var(x),
            parser::Term::Abs(args, body) => {
                args.iter().rev().fold(body.as_ref().into(), |body, arg| {
                    Intermediate::abs(arg, body)
                })
            }
            parser::Term::App(terms) => terms[1..]
                .iter()
                .fold((&terms[0]).into(), |s, t| Intermediate::app(s, t.into())),
            parser::Term::Let(x, s, t) => Intermediate::app(
                Intermediate::abs(x, t.as_ref().into()),
                Intermediate::app(
                    Intermediate::new(r"\f.(\x.f(x x))(\x.f(x x))").unwrap(),
                    Intermediate::abs(x, s.as_ref().into()),
                ),
            ),
            parser::Term::Int(n) => Intermediate::Int(*n),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_standard() {
        assert_eq!(Intermediate::new("x").unwrap(), Intermediate::Var("x"));
        assert_eq!(
            Intermediate::new(r"\x.x").unwrap(),
            Intermediate::abs("x", Intermediate::Var("x"))
        );
        assert_eq!(
            Intermediate::new("x y").unwrap(),
            Intermediate::app(Intermediate::Var("x"), Intermediate::Var("y"))
        );
        assert_eq!(
            Intermediate::new(r"\x.\y.x y").unwrap(),
            Intermediate::abs(
                "x",
                Intermediate::abs(
                    "y",
                    Intermediate::app(Intermediate::Var("x"), Intermediate::Var("y"))
                )
            ),
        );
        assert_eq!(
            Intermediate::new("x y z").unwrap(),
            Intermediate::app(
                Intermediate::app(Intermediate::Var("x"), Intermediate::Var("y")),
                Intermediate::Var("z")
            ),
        );
        assert_eq!(
            Intermediate::new(r"\g.(\x.g (x x)) (\x.g (x x))").unwrap(),
            Intermediate::abs(
                "g",
                Intermediate::app(
                    Intermediate::abs(
                        "x",
                        Intermediate::app(
                            Intermediate::Var("g"),
                            Intermediate::app(Intermediate::Var("x"), Intermediate::Var("x"))
                        )
                    ),
                    Intermediate::abs(
                        "x",
                        Intermediate::app(
                            Intermediate::Var("g"),
                            Intermediate::app(Intermediate::Var("x"), Intermediate::Var("x"))
                        )
                    ),
                ),
            ),
        );
        assert_eq!(
            Intermediate::new(r"\x y z.x y z").unwrap(),
            Intermediate::abs(
                "x",
                Intermediate::abs(
                    "y",
                    Intermediate::abs(
                        "z",
                        Intermediate::app(
                            Intermediate::app(Intermediate::Var("x"), Intermediate::Var("y")),
                            Intermediate::Var("z")
                        )
                    )
                ),
            )
        );
        assert_eq!(
            Intermediate::new(r"\_.x y").unwrap(),
            Intermediate::abs(
                "_",
                Intermediate::app(Intermediate::Var("x"), Intermediate::Var("y"))
            )
        );
        assert_eq!(
            Intermediate::new("# The identity:\n\\x.x").unwrap(),
            Intermediate::abs("x", Intermediate::Var("x")),
        );
    }

    #[test]
    fn test_bruijn() {
        assert_eq!(
            Term::new(r"\x y.x").unwrap(),
            Term::abs(Term::abs(Term::Var(1))),
        );
        assert_eq!(
            Term::new(r"\g.(\x.g (x x)) (\x.g (x x))").unwrap(),
            Term::abs(Term::app(
                Term::abs(Term::app(
                    Term::Var(1),
                    Term::app(Term::Var(0), Term::Var(0)),
                )),
                Term::abs(Term::app(
                    Term::Var(1),
                    Term::app(Term::Var(0), Term::Var(0)),
                ))
            ))
        );
    }

    #[test]
    fn test_reduce() {
        let term = Term::new(r"(\x.x)(\x.x)").unwrap().reduce();
        assert_eq!(term, Term::new(r"\x.x").unwrap());
    }
}
