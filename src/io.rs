use anyhow::{bail, Result};

use crate::term::Bruijn;

fn new_true() -> Bruijn {
    Bruijn::new(r"\trueC falseC.trueC").unwrap()
}

fn new_false() -> Bruijn {
    Bruijn::new(r"\trueC falseC.falseC").unwrap()
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

impl From<u8> for Bruijn {
    fn from(byte: u8) -> Self {
        let mut res = new_nil();
        for i in 0..8 {
            res = cons(
                if (byte >> i) % 2 == 0 {
                    new_false()
                } else {
                    new_true()
                },
                res,
            );
        }
        res
    }
}

impl TryInto<u8> for Bruijn {
    type Error = anyhow::Error;

    fn try_into(mut self) -> Result<u8> {
        let mut res = 0;
        for i in (0..8).rev() {
            let (first, rest) = uncons(self);
            self = rest;

            if first == new_true() {
                res += 1 << i;
            } else if first != new_false() {
                bail!("not a bit");
            }
        }

        Ok(res)
    }
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
        // '\' is 0x5c which is 0b01011100
        assert_eq!(
            Bruijn::from(b'\\'),
            cons(
                new_false(),
                cons(
                    new_true(),
                    cons(
                        new_false(),
                        cons(
                            new_true(),
                            cons(
                                new_true(),
                                cons(new_true(), cons(new_false(), cons(new_false(), new_nil())))
                            )
                        )
                    )
                )
            ),
        );
        assert_eq!(TryInto::<u8>::try_into(Bruijn::from(b'\\')).unwrap(), b'\\');
    }
}
