use std::fmt::Display;

use anyhow::{bail, Result};

use crate::parser;

/// A standard lambda calculus term with named variables and no sugar.
#[derive(PartialEq, Debug)]
enum Standard<'a> {
    Var(&'a str),
    Abs(&'a str, Box<Standard<'a>>),
    App(Box<Standard<'a>>, Box<Standard<'a>>),
}

impl<'a> Standard<'a> {
    fn new(input: &'a str) -> Result<Self> {
        parser::Term::new(input).map(|term| (&term).into())
    }
}

impl<'a> From<&parser::Term<'a>> for Standard<'a> {
    fn from(term: &parser::Term<'a>) -> Self {
        match term {
            parser::Term::Var(x) => Standard::Var(x),
            parser::Term::Abs(args, body) => {
                args.iter().rev().fold(body.as_ref().into(), |body, arg| {
                    Standard::Abs(arg, Box::new(body))
                })
            }
            parser::Term::App(terms) => terms[1..].iter().fold((&terms[0]).into(), |s, t| {
                Standard::App(Box::new(s), Box::new(t.into()))
            }),
        }
    }
}

/// A lambda calculus term with de Bruijn indices (starting at 0). The Options are always Some.
/// They're only optional so we can `take` them while reducing to avoid some clones.
#[derive(Clone, PartialEq, Debug)]
pub enum Bruijn {
    Var(usize),
    Abs(Box<Option<Bruijn>>),
    App(Box<Bruijn>, Box<Option<Bruijn>>),
}

impl Display for Bruijn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Bruijn::Var(x) => write!(f, "{x}")?,
            Bruijn::Abs(body) => write!(f, r"(\{})", body.as_ref().as_ref().unwrap())?,
            Bruijn::App(s, t) => write!(f, r"({} {})", s, t.as_ref().as_ref().unwrap())?,
        }
        Ok(())
    }
}

impl Bruijn {
    pub fn new(s: &str) -> Result<Self> {
        Standard::new(s).and_then(|term| (&term).try_into())
    }

    #[must_use]
    pub fn abs(body: Self) -> Self {
        Self::Abs(Box::new(Some(body)))
    }

    #[must_use]
    pub fn app(s: Self, t: Self) -> Self {
        Self::App(Box::new(s), Box::new(Some(t)))
    }

    pub fn abs_into_body(self) -> Result<Self> {
        let Self::Abs(body) = self else {
            bail!("not an abstraction");
        };
        Ok(body.unwrap())
    }

    pub fn app_into_pair(self) -> Result<(Self, Self)> {
        let Self::App(s, t) = self else {
            bail!("not an application");
        };
        Ok((*s, (*t).unwrap()))
    }

    #[must_use]
    pub fn reduce(mut self) -> Self {
        fn substitute<'a>(
            term: &'a mut Bruijn,
            // Exactly one of param or param_ref will be Some. TODO can we enforce this? Cow is
            // almost the right type but not quite.
            param: &mut Option<Bruijn>,
            param_ref: &mut Option<&'a Bruijn>,
            depth: usize,
        ) {
            match term {
                Bruijn::Var(x) => {
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
                Bruijn::Abs(body) => {
                    substitute(body.as_mut().as_mut().unwrap(), param, param_ref, depth + 1);
                }
                Bruijn::App(s, t) => {
                    substitute(s, param, param_ref, depth);
                    substitute(t.as_mut().as_mut().unwrap(), param, param_ref, depth);
                }
            }
        }

        fn reduce_once(term: &mut Bruijn) -> bool {
            match term {
                Bruijn::Var(_) => false,
                Bruijn::Abs(body) => reduce_once(body.as_mut().as_mut().unwrap()),
                Bruijn::App(s, t) => {
                    if let Bruijn::Abs(body) = s.as_mut() {
                        // body and t are owned by term so are dropped when we re-assign. The None
                        // are never reachable.
                        substitute(body.as_mut().as_mut().unwrap(), t.as_mut(), &mut None, 0);
                        *term = body.take().unwrap();
                        true
                    } else {
                        reduce_once(s) || reduce_once(t.as_mut().as_mut().unwrap())
                    }
                }
            }
        }

        while reduce_once(&mut self) {}

        self
    }
}

impl<'a> TryFrom<&Standard<'a>> for Bruijn {
    type Error = anyhow::Error;

    fn try_from(term: &Standard<'a>) -> Result<Self> {
        fn try_from_with_scope<'a>(scope: &mut Vec<&'a str>, term: &'a Standard) -> Result<Bruijn> {
            match term {
                Standard::Var(x) => {
                    if let Some((i, _)) = scope.iter().rev().enumerate().find(|&(_, v)| x == v) {
                        Ok(Bruijn::Var(i))
                    } else {
                        bail!("unbound variable {x}");
                    }
                }
                Standard::Abs(arg, body) => {
                    scope.push(arg);
                    let res = Bruijn::abs(try_from_with_scope(scope, body)?);
                    scope.pop();
                    Ok(res)
                }
                Standard::App(s, t) => Ok(Bruijn::app(
                    try_from_with_scope(scope, s)?,
                    try_from_with_scope(scope, t)?,
                )),
            }
        }

        try_from_with_scope(&mut Vec::new(), term)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_standard() {
        fn var(var: &str) -> Standard {
            Standard::Var(var)
        }

        fn abs<'a>(var: &'a str, body: Standard<'a>) -> Standard<'a> {
            Standard::Abs(var, Box::new(body))
        }

        fn app<'a>(s: Standard<'a>, t: Standard<'a>) -> Standard<'a> {
            Standard::App(Box::new(s), Box::new(t))
        }

        assert_eq!(Standard::new("x").unwrap(), var("x"));
        assert_eq!(Standard::new(r"\x.x").unwrap(), abs("x", var("x")));
        assert_eq!(Standard::new("x y").unwrap(), app(var("x"), var("y")));
        assert_eq!(
            Standard::new(r"\x.\y.x y").unwrap(),
            abs("x", abs("y", app(var("x"), var("y")))),
        );
        assert_eq!(
            Standard::new("x y z").unwrap(),
            app(app(var("x"), var("y")), var("z")),
        );
        assert_eq!(
            Standard::new(r"\g.(\x.g (x x)) (\x.g (x x))").unwrap(),
            abs(
                "g",
                app(
                    abs("x", app(var("g"), app(var("x"), var("x")))),
                    abs("x", app(var("g"), app(var("x"), var("x")))),
                ),
            ),
        );
        assert_eq!(
            Standard::new(r"\x y z.x y z").unwrap(),
            abs(
                "x",
                abs("y", abs("z", app(app(var("x"), var("y")), var("z")))),
            )
        );
        assert_eq!(
            Standard::new(r"\_.x y").unwrap(),
            abs("_", app(var("x"), var("y")))
        );
        assert_eq!(
            Standard::new("# The identity:\n\\x.x").unwrap(),
            abs("x", var("x")),
        );
    }

    #[test]
    fn test_bruijn() {
        assert_eq!(
            Bruijn::new(r"\x y.x").unwrap(),
            Bruijn::abs(Bruijn::abs(Bruijn::Var(1)))
        );
        assert_eq!(
            Bruijn::new(r"\g.(\x.g (x x)) (\x.g (x x))").unwrap(),
            Bruijn::abs(Bruijn::app(
                Bruijn::abs(Bruijn::app(
                    Bruijn::Var(1),
                    Bruijn::app(Bruijn::Var(0), Bruijn::Var(0))
                )),
                Bruijn::abs(Bruijn::app(
                    Bruijn::Var(1),
                    Bruijn::app(Bruijn::Var(0), Bruijn::Var(0))
                ))
            ))
        );
    }

    #[test]
    fn test_reduce() {
        let term = Bruijn::new(r"(\x.x)(\x.x)").unwrap().reduce();
        assert_eq!(term, Bruijn::new(r"\x.x").unwrap());
    }
}
