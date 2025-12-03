pub struct Day02<'a> {
    parsed: Data<'a>,
}

impl<'a> super::Day for Day02<'a> {
    const DAY_NUMBER: usize = 2;

    fn parse<'c, 'b : 'c>(input: &'b str) -> Self where Self: 'c {
        Self {
            parsed: parse(input),
        }
    }
    fn part1(&self) -> u64 {
        u64::from(part1(&self.parsed))
    }
    fn part2(&self) -> u64 {
        u64::from(part2(&self.parsed))
    }
}

fn parse<'a>(input: &'a str) -> Vec<(&'a str, &'a str)> {
    input
        .lines()
        .map(|line| {
            line.split_once('-').unwrap()
        })
        .collect()
}

type Data<'a> = Vec<(&'a str, &'a str)>;

fn part1<'a>(data: &[(&str, &str)]) -> u32 {
    0
}

fn part2<'a>(data: &[(&str, &str)]) -> u32 {
    0
}