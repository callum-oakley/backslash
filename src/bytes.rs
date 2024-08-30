use anyhow::{Context, Result};

use crate::{int, list, term::Term};

pub fn encode(bytes: &[u8]) -> Term {
    list::encode(bytes.iter().map(|&byte| int::encode(i64::from(byte))))
}

pub fn decode(term: Term) -> Result<Vec<u8>> {
    list::decode(term)
        .context("decoding list")?
        .into_iter()
        .map(|term| {
            int::decode(term)
                .context("decoding int")
                .and_then(|n| u8::try_from(n).map_err(Into::into))
        })
        .collect()
}
