use crate::term::Bruijn;

pub fn new_true() -> Bruijn {
    Bruijn::new(r"\x y.x").unwrap()
}

pub fn new_false() -> Bruijn {
    Bruijn::new(r"\x y.y").unwrap()
}

pub fn new_pair() -> Bruijn {
    Bruijn::new(r"\x y s.s x y").unwrap()
}

pub fn new_first() -> Bruijn {
    Bruijn::new(r"\p.p \x y.x").unwrap()
}

pub fn new_rest() -> Bruijn {
    Bruijn::new(r"\p.p \x y.y").unwrap()
}
