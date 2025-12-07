pub struct Day02 {
    parsed: Vec<Range>,
}

impl super::Day for Day02 {
    const DAY_NUMBER: usize = 2;

    fn parse(input: &str) -> Self {
        let input = input.trim();

        let parsed = input
            .split(',')
            .map(|range_str| {
                let (a, b) = range_str.split_at(range_str.find('-').expect("Range is missing '-'"));
                let b = &b[1..];
                Range {
                    low: a.parse().expect("Range low parse failed"),
                    high: b.parse().expect("Range high parse failed"),
                }
            })
            .collect();
        Self { parsed }
    }
    fn part1(&self) -> u64 {
        self.parsed
            .iter()
            .flat_map(|r| r.low..=r.high)
            .filter(|i| is_invalid_id(*i))
            .sum()
    }
    fn part2(&self) -> u64 {
        0
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
struct Range {
    low: u64,
    high: u64,
}

fn is_invalid_id(n: u64) -> bool {
    // An invalid ID is one that is a string of digits repeated twice.
    // I don't want to convert to/from strings - do some maths instead

    // A number "abcdef" is invalid if "abc" == "def".
    // That is, "abcdef" == "def" * 1001
    // Or,      "abcdef" / 1001 < 1000.

    // How do we get this 1001? This would be different for different length numbers...
    // Have some number N.
    // Start with the length of `n` in base 10.
    let digit_count = n.ilog10() + 1;

    // For part1, this must be able to be split into 2, so let's check that
    if digit_count % 2 != 0 {
        return false;
    }

    let half_digit_count = digit_count / 2;

    // This is the number we are testing `n` with - in the example above it is 1001.
    let magic_num = (10u64.pow(digit_count) - 1) / (10u64.pow(half_digit_count) - 1);

    n % magic_num == 0
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_is_invalid_id() {
        assert!(is_invalid_id(11));
        assert!(is_invalid_id(22));
        assert!(!is_invalid_id(24));
        assert!(is_invalid_id(1010));
        assert!(!is_invalid_id(1011));
        assert!(is_invalid_id(123123));
        assert!(!is_invalid_id(123321));
        assert!(!is_invalid_id(111));
    }
}
