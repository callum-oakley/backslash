use lazy_static::lazy_static;

use crate::Bruijn;

lazy_static! {
    pub static ref TRUE: Bruijn = r"\x y.x".parse().unwrap();
    pub static ref FALSE: Bruijn = r"\x y.y".parse().unwrap();

    // Pairs and lists (TODO re-use TRUE and FALSE in the below when that's supported).
    pub static ref PAIR: Bruijn = r"\x y s.s x y".parse().unwrap();
    pub static ref FIRST: Bruijn = r"\p.p \x y.x".parse().unwrap();
    pub static ref REST: Bruijn = r"\p.p \x y.y".parse().unwrap();
    pub static ref NIL: Bruijn = r"\x y.y".parse().unwrap();
}
