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
        // self.parsed
        //     .iter()
        //     .flat_map(|r| r.low..=r.high)
        //     .filter(|i| is_invalid_id(*i))
        //     .sum()
        self.parsed.iter().map(count_invalids).sum()
    }
    fn part2(&self) -> u64 {
        self.parsed
            .iter()
            .flat_map(|r| r.low..=r.high)
            .filter(|i| is_invalid_id2(*i))
            .sum()
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
fn is_invalid_id2(n: u64) -> bool {
    // Like `is_invalid_id`, but this time we consider all possible number of repetitions, not just 2

    let digit_count = n.ilog10() + 1;

    for reps in 2..=digit_count {
        // Now replace 2 with reps
        if digit_count % reps != 0 {
            continue;
        }

        let half_digit_count = digit_count / reps;

        // This is the number we are testing `n` with - in the example above it is 1001.
        let magic_num = (10u64.pow(digit_count) - 1) / (10u64.pow(half_digit_count) - 1);

        if n % magic_num == 0 {
            return true;
        }
    }

    false
}

fn count_invalids(range: &Range) -> u64 {
    let low_digit_count = range.low.ilog10() + 1;
    let high_digit_count = range.high.ilog10() + 1;

    println!("low: {}, high: {}", range.low, range.high);

    let values = (low_digit_count..=high_digit_count)
        .flat_map(move |digit_count| {
            {
                println!("digit_count: {digit_count}");

                let lower_bound = range.low.max(10u64.pow(digit_count - 1));
                let upper_bound = range.high.min(10u64.pow(digit_count) - 1);

                divisors(digit_count).flat_map(move |reps| {
                    let split_digit_count = digit_count / reps;
                    let magic_num =
                        (10u64.pow(digit_count) - 1) / (10u64.pow(split_digit_count) - 1);

                    // Want the multiples of magic_num within lower_bound..=upper_bound
                    // Start from the least multiple of `magic_num` that is at least `lower_bound`
                    let start = match lower_bound % magic_num {
                        0 => lower_bound,
                        x => lower_bound - x + magic_num,
                    };
                    (start..=upper_bound).step_by(magic_num as usize)
                })
            }
        })
        .collect::<hashbrown::HashSet<_>>();

    dbg!(&values);

    values.len() as u64
}

/// The non-trivial divisors of n.
fn divisors(n: u32) -> impl Iterator<Item = u32> {
    (2..=(n / 2)).filter(move |i| n % *i == 0).chain([n])
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
    #[test]
    fn test_is_invalid_id2() {
        assert!(is_invalid_id2(11));
        assert!(is_invalid_id2(22));
        assert!(!is_invalid_id2(24));
        assert!(is_invalid_id2(1010));
        assert!(!is_invalid_id2(1011));
        assert!(is_invalid_id2(123123));
        assert!(!is_invalid_id2(123321));

        assert!(is_invalid_id2(111));

        assert!(is_invalid_id2(11));
        assert!(is_invalid_id2(22));
        assert!(is_invalid_id2(99));
        assert!(is_invalid_id2(111));
        assert!(is_invalid_id2(999));
        assert!(is_invalid_id2(1010));
        assert!(is_invalid_id2(1188511885));
        assert!(is_invalid_id2(222222));
        assert!(is_invalid_id2(446446));
        assert!(is_invalid_id2(38593859));
        assert!(is_invalid_id2(565656));
        assert!(is_invalid_id2(824824824));
        assert!(is_invalid_id2(2121212121));
    }

    #[test]
    fn test_count_invalids() {
        assert_eq!(count_invalids(&Range { low: 11, high: 22 }), 2);
        assert_eq!(count_invalids(&Range { low: 95, high: 115 }), 1);
        assert_eq!(
            count_invalids(&Range {
                low: 998,
                high: 1012
            }),
            1
        );
        assert_eq!(
            count_invalids(&Range {
                low: 1188511880,
                high: 1188511890
            }),
            1
        );
        assert_eq!(
            count_invalids(&Range {
                low: 222220,
                high: 222224
            }),
            1
        );
        assert_eq!(
            count_invalids(&Range {
                low: 1698522,
                high: 1698528
            }),
            0
        );
        assert_eq!(
            count_invalids(&Range {
                low: 446443,
                high: 446449
            }),
            1
        );
        assert_eq!(
            count_invalids(&Range {
                low: 38593856,
                high: 38593862
            }),
            1
        );
    }
}
