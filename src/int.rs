use anyhow::{bail, Result};

use crate::term::Term;

pub fn encode(n: i64) -> Term {
    let mut body = Term::Var(3);
    for trit in to_balanced_ternary(n).into_iter().rev() {
        body = Term::app(
            match trit {
                -1 => Term::Var(2),
                0 => Term::Var(1),
                1 => Term::Var(0),
                _ => panic!("invalid trit: {trit}"),
            },
            body,
        );
    }
    Term::abs(Term::abs(Term::abs(Term::abs(body))))
}

pub fn decode(mut term: Term) -> Result<i64> {
    let mut trits = Vec::new();
    term = term
        .decode_abs()?
        .decode_abs()?
        .decode_abs()?
        .decode_abs()?;
    while term != Term::Var(3) {
        let (s, t) = term.decode_app()?;
        trits.push(match s {
            Term::Var(2) => -1,
            Term::Var(1) => 0,
            Term::Var(0) => 1,
            _ => bail!("invalid trit"),
        });
        term = t;
    }
    Ok(from_balanced_ternary(trits))
}

fn to_balanced_ternary(mut n: i64) -> Vec<i8> {
    // Little-endian
    let mut res = vec![0i8];
    for i in 0.. {
        res.push(0);
        let r = i8::try_from(n % 3).unwrap();
        res[i] += r;
        if res[i] > 1 {
            res[i] -= 3;
            res[i + 1] = 1;
        } else if res[i] < -1 {
            res[i] += 3;
            res[i + 1] = -1;
        }
        n /= 3;
        if n == 0 {
            break;
        }
    }

    // Remove trailing zeros
    while res.last() == Some(&0) {
        res.pop();
    }

    res
}

fn from_balanced_ternary(trits: Vec<i8>) -> i64 {
    let mut n = 0;
    for (i, trit) in trits.into_iter().enumerate() {
        match trit {
            -1 => n -= 3i64.pow(i.try_into().unwrap()),
            0 => (),
            1 => n += 3i64.pow(i.try_into().unwrap()),
            _ => panic!("invalid trit: {trit}"),
        }
    }
    n
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int() {
        assert_eq!(to_balanced_ternary(-13), vec![-1, -1, -1]);
        assert_eq!(to_balanced_ternary(-12), vec![0, -1, -1]);
        assert_eq!(to_balanced_ternary(-11), vec![1, -1, -1]);
        assert_eq!(to_balanced_ternary(-10), vec![-1, 0, -1]);
        assert_eq!(to_balanced_ternary(-9), vec![0, 0, -1]);
        assert_eq!(to_balanced_ternary(-8), vec![1, 0, -1]);
        assert_eq!(to_balanced_ternary(-7), vec![-1, 1, -1]);
        assert_eq!(to_balanced_ternary(-6), vec![0, 1, -1]);
        assert_eq!(to_balanced_ternary(-5), vec![1, 1, -1]);
        assert_eq!(to_balanced_ternary(-4), vec![-1, -1]);
        assert_eq!(to_balanced_ternary(-3), vec![0, -1]);
        assert_eq!(to_balanced_ternary(-2), vec![1, -1]);
        assert_eq!(to_balanced_ternary(-1), vec![-1]);
        assert_eq!(to_balanced_ternary(0), vec![]);
        assert_eq!(to_balanced_ternary(1), vec![1]);
        assert_eq!(to_balanced_ternary(2), vec![-1, 1]);
        assert_eq!(to_balanced_ternary(3), vec![0, 1]);
        assert_eq!(to_balanced_ternary(4), vec![1, 1]);
        assert_eq!(to_balanced_ternary(5), vec![-1, -1, 1]);
        assert_eq!(to_balanced_ternary(6), vec![0, -1, 1]);
        assert_eq!(to_balanced_ternary(7), vec![1, -1, 1]);
        assert_eq!(to_balanced_ternary(8), vec![-1, 0, 1]);
        assert_eq!(to_balanced_ternary(9), vec![0, 0, 1]);
        assert_eq!(to_balanced_ternary(10), vec![1, 0, 1]);
        assert_eq!(to_balanced_ternary(11), vec![-1, 1, 1]);
        assert_eq!(to_balanced_ternary(12), vec![0, 1, 1]);
        assert_eq!(to_balanced_ternary(13), vec![1, 1, 1]);

        for n in -1000..1000 {
            assert_eq!(from_balanced_ternary(to_balanced_ternary(n)), n);
            assert_eq!(decode(encode(n)).unwrap(), n);
        }
    }
}
