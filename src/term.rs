use std::mem;

use anyhow::{bail, Result};

/// A pure lambda calculus term with de Bruijn indices starting at 0.
#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl Term {
    pub fn abs(body: Self) -> Self {
        Term::Abs(Box::new(body))
    }

    pub fn app(s: Self, t: Self) -> Self {
        Term::App(Box::new(s), Box::new(t))
    }

    pub fn try_unabs(self) -> Result<Self> {
        if let Term::Abs(body) = self {
            Ok(*body)
        } else {
            bail!("not an abstraction");
        }
    }

    pub fn try_unapp(self) -> Result<(Self, Self)> {
        if let Term::App(s, t) = self {
            Ok((*s, *t))
        } else {
            bail!("not an application");
        }
    }

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
