use std::str::FromStr;

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

/// A lambda calculus term with de Bruijn indices (starting at 0).
#[derive(Clone, PartialEq, Debug)]
pub enum Bruijn {
    Var(usize),
    /// Always Some. Only optional so we can `take` it in `reduce`.
    Abs(Box<Option<Bruijn>>),
    App(Box<Bruijn>, Box<Bruijn>),
}

impl Bruijn {
    fn reduce(&mut self) {
        fn substitute(term: &mut Bruijn, param: &Bruijn, depth: usize) {
            match term {
                Bruijn::Var(x) => {
                    if *x == depth {
                        *term = param.clone();
                    }
                }
                Bruijn::Abs(body) => {
                    substitute(body.as_mut().as_mut().unwrap(), param, depth + 1);
                }
                Bruijn::App(s, t) => {
                    substitute(s, param, depth);
                    substitute(t, param, depth);
                }
            }
        }

        fn reduce_once(term: &mut Bruijn) -> bool {
            match term {
                Bruijn::Var(_) => false,
                Bruijn::Abs(body) => reduce_once(body.as_mut().as_mut().unwrap()),
                Bruijn::App(s, t) => {
                    if let Bruijn::Abs(body) = s.as_mut() {
                        substitute(body.as_mut().as_mut().unwrap(), t, 0);
                        // body is owned by term so is dropped when we re-assign. The None is never
                        // reachable.
                        *term = body.take().unwrap();
                        true
                    } else {
                        reduce_once(s) || reduce_once(t)
                    }
                }
            }
        }

        while reduce_once(self) {}
    }
}

impl FromStr for Bruijn {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        Standard::new(s).and_then(|term| (&term).try_into())
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
                    let res = Bruijn::Abs(Box::new(Some(try_from_with_scope(scope, body)?)));
                    scope.pop();
                    Ok(res)
                }
                Standard::App(s, t) => Ok(Bruijn::App(
                    Box::new(try_from_with_scope(scope, s)?),
                    Box::new(try_from_with_scope(scope, t)?),
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
        fn var(var: usize) -> Bruijn {
            Bruijn::Var(var)
        }

        fn abs(body: Bruijn) -> Bruijn {
            Bruijn::Abs(Box::new(Some(body)))
        }

        fn app(s: Bruijn, t: Bruijn) -> Bruijn {
            Bruijn::App(Box::new(s), Box::new(t))
        }

        assert_eq!(r"\x y.x".parse::<Bruijn>().unwrap(), abs(abs(var(1))));
        assert_eq!(
            r"\g.(\x.g (x x)) (\x.g (x x))".parse::<Bruijn>().unwrap(),
            abs(app(
                abs(app(var(1), app(var(0), var(0)))),
                abs(app(var(1), app(var(0), var(0))))
            ))
        )
    }

    #[test]
    fn test_reduce() {
        let mut term: Bruijn = r"(\x.x)(\x.x)".parse().unwrap();
        term.reduce();
        assert_eq!(term, r"\x.x".parse().unwrap());
    }
}
