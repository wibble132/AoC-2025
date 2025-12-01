pub struct Day01 {
    parsed: Vec<Rotation>,
}

#[derive(Clone, Copy)]
enum Dir {
    L,
    R,
}

struct Rotation(Dir, u64);

fn parse(input: &str) -> Vec<Rotation> {
    input.lines().map(parse_line).collect::<Vec<_>>()
}

fn parse_line(line: &str) -> Rotation {
    let mut chars = line.chars();
    let dir = chars.next().unwrap();
    let number = chars.as_str().parse::<u64>().unwrap();
    match dir {
        'L' => Rotation(Dir::L, number),
        'R' => Rotation(Dir::R, number),
        _ => unreachable!(),
    }
}

const fn spin(rot: &Rotation, pos: u64) -> u64 {
    let pos = match rot.0 {
        Dir::R => pos + rot.1,
        Dir::L => pos + (100 - (rot.1 % 100)),
    };

    pos % 100
}

fn part1(data: &[Rotation]) -> u64 {
    let mut pos = 50;
    let mut count = 0;
    for rot in data {
        pos = spin(rot, pos);
        if pos == 0 {
            count += 1;
        }
    }
    count
}

fn part2(data: &[Rotation]) -> u64 {
    let mut pos = 50;
    let mut count = 0;
    for rot in data {
        let rotation = &Rotation(rot.0, 1);
        for _ in 0..rot.1 {
            pos = spin(rotation, pos);
            if pos == 0 {
                count += 1;
            }
        }
    }
    count
}

impl super::Day for Day01 {
    const DAY_NUMBER: usize = 1;

    fn parse(input: &str) -> Self {
        Self {
            parsed: parse(input),
        }
    }
    fn part1(&self) -> u64 {
        part1(&self.parsed)
    }
    fn part2(&self) -> u64 {
        part2(&self.parsed)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_spin() {
        assert_eq!(spin(&Rotation(Dir::R, 3), 5), 8);
        assert_eq!(spin(&Rotation(Dir::L, 3), 5), 2);
        assert_eq!(spin(&Rotation(Dir::L, 10), 5), 95);
        assert_eq!(spin(&Rotation(Dir::R, 10), 95), 5);
    }

    #[test]
    fn example_1() {
        let input = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
";

        let (p1, p2) = crate::evaluate_day::<Day01>(&input);
        assert_eq!(p1, 3);
        assert_eq!(p2, 6);
    }
}
