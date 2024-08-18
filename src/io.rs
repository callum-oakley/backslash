use anyhow::{bail, Result};

use crate::term::Bruijn;

impl From<u8> for Bruijn {
    fn from(mut byte: u8) -> Self {
        let mut balanced_ternary = [0i8; 6];
        for i in (0..6).rev() {
            balanced_ternary[i] += i8::try_from(byte % 3).unwrap();
            if balanced_ternary[i] > 1 {
                balanced_ternary[i] -= 3;
                balanced_ternary[i - 1] = 1;
            }
            byte /= 3;
        }

        // Encode as \z x- x0 x1.body (so z, x-, x0, x1 have Bruijn indices 3, 2, 1, 0)
        let mut body = Bruijn::Var(3);
        for trit in balanced_ternary.into_iter().skip_while(|&trit| trit == 0) {
            body = Bruijn::app(
                match trit {
                    -1 => Bruijn::Var(2),
                    0 => Bruijn::Var(1),
                    1 => Bruijn::Var(0),
                    _ => panic!("invalid trit: {trit}"),
                },
                body,
            );
        }

        Bruijn::abs(Bruijn::abs(Bruijn::abs(Bruijn::abs(body))))
    }
}

impl TryInto<u8> for Bruijn {
    type Error = anyhow::Error;

    fn try_into(mut self) -> Result<u8> {
        let mut balanced_ternary = [0i8; 6];
        self = self.try_unabs()?.try_unabs()?.try_unabs()?.try_unabs()?;
        for i in (0..6).rev() {
            let (s, t) = self.try_unapp()?;
            balanced_ternary[i] = match s {
                Bruijn::Var(2) => -1,
                Bruijn::Var(1) => 0,
                Bruijn::Var(0) => 1,
                _ => bail!("malformed byte"),
            };
            self = t;
            if self == Bruijn::Var(3) {
                break;
            }
        }
        assert!(self == Bruijn::Var(3), "too many trits");

        // TODO this will currently underflow if balanced_ternary represents a negative number
        let mut res = 0;
        for (i, trit) in balanced_ternary.into_iter().rev().enumerate().rev() {
            match trit {
                -1 => res -= 3_u8.pow(i.try_into().unwrap()),
                0 => (),
                1 => res += 3_u8.pow(i.try_into().unwrap()),
                _ => panic!("invalid trit: {trit}"),
            }
        }

        Ok(res)
    }
}

fn new_nil() -> Bruijn {
    Bruijn::new(r"\nilC consC.nilC").unwrap()
}

fn new_cons() -> Bruijn {
    Bruijn::new(r"\head tail.\nilC consC.consC head tail").unwrap()
}

fn new_head() -> Bruijn {
    Bruijn::new(r"\list.list (\nilC consC.nilC) \head tail.head").unwrap()
}

fn new_tail() -> Bruijn {
    Bruijn::new(r"\list.list (\nilC consC.nilC) \head tail.tail").unwrap()
}

fn cons(a: Bruijn, b: Bruijn) -> Bruijn {
    Bruijn::app(Bruijn::app(new_cons(), a), b).reduce()
}

fn uncons(term: Bruijn) -> (Bruijn, Bruijn) {
    (
        Bruijn::app(new_head(), term.clone()).reduce(),
        Bruijn::app(new_tail(), term).reduce(),
    )
}

impl From<&[u8]> for Bruijn {
    fn from(bytes: &[u8]) -> Self {
        fn from_iter(bytes: &mut impl Iterator<Item = u8>) -> Bruijn {
            match bytes.next() {
                Some(byte) => cons(byte.into(), from_iter(bytes)),
                None => new_nil(),
            }
        }
        from_iter(&mut bytes.iter().copied())
    }
}

impl TryInto<Vec<u8>> for Bruijn {
    type Error = anyhow::Error;

    fn try_into(mut self) -> Result<Vec<u8>> {
        let mut res = Vec::new();
        while self != new_nil() {
            let (first, rest) = uncons(self);
            self = rest;
            res.push(first.try_into()?);
        }
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encoding_and_decoding() {
        assert_eq!(
            Bruijn::from(2).to_string(),
            r"(\a.(\b.(\c.(\d.(b (d a))))))",
        );
        assert_eq!(
            Bruijn::from(5).to_string(),
            r"(\a.(\b.(\c.(\d.(b (b (d a)))))))",
        );
        assert_eq!(
            Bruijn::from(0xff).to_string(),
            r"(\a.(\b.(\c.(\d.(c (d (d (c (c (d a))))))))))",
        );
        assert_eq!(TryInto::<u8>::try_into(Bruijn::from(2)).unwrap(), 2);
        assert_eq!(TryInto::<u8>::try_into(Bruijn::from(5)).unwrap(), 5);
        assert_eq!(TryInto::<u8>::try_into(Bruijn::from(0xff)).unwrap(), 0xff);
    }
}
