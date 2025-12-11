use std::hint::assert_unchecked;

pub struct Day07 {
    parsed: Data,
}

impl super::Day for Day07 {
    const DAY_NUMBER: usize = 7;

    fn parse(input: &str) -> Self {
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

struct Data {
    start_pos: usize,
    splitters: Vec<Vec<usize>>,
}

fn parse(input: &str) -> Data {
    for line in input.lines() {
        assert!(line.len() <= LINE_LEN);
    }

    let (start_line, splitter_lines) = input.split_once('\n').unwrap();

    let start_pos = start_line.find('S').unwrap();
    let splitter_lines = splitter_lines.lines();
    let splitters: Vec<Vec<usize>> = splitter_lines
        .map(|line| {
            line.chars()
                .enumerate()
                .filter(|&(_, c)| c == '^')
                .map(|(i, _)| i)
                .collect()
        })
        .filter(|line: &Vec<usize>| line.len() > 0)
        .collect();

    Data {
        start_pos,
        splitters,
    }
}

const LINE_LEN: usize = 142;

fn part1(data: &Data) -> u64 {
    // Safety: Verified in `parse`
    unsafe { assert_unchecked(data.start_pos + 2 < LINE_LEN); }
    let mut beams: [bool; LINE_LEN] = [false; _];
    beams[data.start_pos] = true;
    let mut count = 0;

    for line in &data.splitters {
        for splitter in line {
            // Safety: Verified in `parse`
            unsafe { assert_unchecked(*splitter + 2 < LINE_LEN); }
            if beams[*splitter] {
                beams[*splitter - 1] = true;
                beams[*splitter] = false;
                beams[*splitter + 1] = true;
                count += 1;
            }
        }
    }

    count
}

fn part2(data: &Data) -> u64 {
    // Safety: Verified in `parse`
    unsafe { assert_unchecked(data.start_pos + 2 < LINE_LEN); }
    let mut counts: [u64; LINE_LEN] = [0; _];
    counts[data.start_pos] = 1;

    for line in &data.splitters {
        for splitter in line {
            // Safety: Verified in `parse`
            unsafe { assert_unchecked(*splitter + 2 < LINE_LEN); }
            let in_count = counts[*splitter];

            counts[*splitter - 1] += in_count;
            counts[*splitter + 1] += in_count;
            counts[*splitter] = 0;
        }
    }

    counts.into_iter().sum()
}
