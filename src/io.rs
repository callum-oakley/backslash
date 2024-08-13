use std::io::{self, Read, Stdin};

use anyhow::{bail, Result};

use crate::{constants, term::Bruijn};

impl From<u8> for Bruijn {
    fn from(byte: u8) -> Self {
        let mut res = constants::new_false();
        for i in 0..8 {
            res = Bruijn::pair(
                if (byte >> i) % 2 == 0 {
                    constants::new_false()
                } else {
                    constants::new_true()
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
            let (first, rest) = self.unpair();
            self = rest;

            if first == constants::new_true() {
                res += 1 << i;
            } else if first != constants::new_false() {
                bail!("neither true nor false");
            }
        }

        Ok(res)
    }
}

impl TryFrom<Stdin> for Bruijn {
    type Error = anyhow::Error;

    fn try_from(stdin: Stdin) -> Result<Self> {
        fn from_bytes(bytes: &mut impl Iterator<Item = io::Result<u8>>) -> Result<Bruijn> {
            match bytes.next() {
                Some(byte) => Ok(Bruijn::pair(byte?.into(), from_bytes(bytes)?)),
                None => Ok(constants::new_false()),
            }
        }
        from_bytes(&mut stdin.bytes())
    }
}

impl TryInto<Vec<u8>> for Bruijn {
    type Error = anyhow::Error;

    fn try_into(mut self) -> Result<Vec<u8>> {
        let mut res = Vec::new();
        while self != constants::new_false() {
            let (first, rest) = self.unpair();
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
        // b'\' is 0x5c which is 0b01011100
        assert_eq!(
            Bruijn::from(b'\\'),
            Bruijn::pair(
                constants::new_false(),
                Bruijn::pair(
                    constants::new_true(),
                    Bruijn::pair(
                        constants::new_false(),
                        Bruijn::pair(
                            constants::new_true(),
                            Bruijn::pair(
                                constants::new_true(),
                                Bruijn::pair(
                                    constants::new_true(),
                                    Bruijn::pair(
                                        constants::new_false(),
                                        Bruijn::pair(
                                            constants::new_false(),
                                            constants::new_false(),
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
        );
        assert_eq!(TryInto::<u8>::try_into(Bruijn::from(b'\\')).unwrap(), b'\\');
    }
}
