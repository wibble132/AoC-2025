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
    line_length: usize,
}

fn parse(input: &str) -> Data {
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
    let line_length = start_line.len();

    Data {
        start_pos,
        splitters,
        line_length,
    }
}

fn part1(data: &Data) -> u64 {
    let mut beams = vec![false; data.line_length];
    beams[data.start_pos] = true;
    let mut count = 0;

    for line in &data.splitters {
        for splitter in line {
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
    0
}
