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
        parser::Term::new(s).and_then(|term| (&term).try_into())
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

impl<'a> TryFrom<&parser::Term<'a>> for Term {
    type Error = anyhow::Error;

    fn try_from(term: &parser::Term<'a>) -> Result<Self> {
        fn try_from_with_scope<'a>(
            scope: &mut Vec<&'a str>,
            term: &'a parser::Term,
        ) -> Result<Term> {
            match term {
                parser::Term::Var(x) => {
                    if let Some((i, _)) = scope.iter().rev().enumerate().find(|&(_, v)| x == v) {
                        Ok(Term::Var(i))
                    } else {
                        bail!("unbound variable {x}");
                    }
                }
                parser::Term::Abs(arg, body) => {
                    scope.push(arg);
                    let res = Term::abs(try_from_with_scope(scope, body)?);
                    scope.pop();
                    Ok(res)
                }
                parser::Term::App(s, t) => Ok(Term::app(
                    try_from_with_scope(scope, s)?,
                    try_from_with_scope(scope, t)?,
                )),
                parser::Term::Int(n) => Ok(int::encode(*n)),
            }
        }

        try_from_with_scope(&mut Vec::new(), term)
    }
}
