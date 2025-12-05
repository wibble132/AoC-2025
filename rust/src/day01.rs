pub struct Day01 {
    parsed: Vec<Rotation>,
}

impl super::Day for Day01 {
    const DAY_NUMBER: usize = 1;

    fn parse(input: &str) -> Self {
        Self {
            parsed: parse(&input),
        }
    }
    fn part1(&self) -> u64 {
        u64::from(part1(&self.parsed))
    }
    fn part2(&self) -> u64 {
        u64::from(part2(&self.parsed))
    }
}

enum Dir {
    L,
    R,
}

struct Rotation(Dir, u16);

fn parse(input: &str) -> Vec<Rotation> {
    input.lines().map(parse_line).collect::<Vec<_>>()
}

fn parse_line(line: &str) -> Rotation {
    let mut chars = line.chars();
    let dir = chars.next().unwrap();
    let number = chars.as_str().parse::<u16>().unwrap();
    match dir {
        'L' => Rotation(Dir::L, number),
        'R' => Rotation(Dir::R, number),
        _ => unreachable!(),
    }
}

const fn spin(rot: &Rotation, pos: u16) -> (u16, u16) {
    match rot.0 {
        Dir::R => {
            let end_pos = pos + rot.1;
            (end_pos / 100, end_pos % 100)
        }
        Dir::L => {
            let (rots, extra) = (rot.1 / 100, rot.1 % 100);
            if extra > pos {
                (rots + 1, pos + 100 - extra)
            } else {
                (rots, pos - extra)
            }
        }
    }
}

fn part1(data: &[Rotation]) -> u16 {
    let mut pos = 50;
    let mut count = 0;
    for rot in data {
        // (_, pos) = spin(rot, pos);
        // if pos == 0 {
        //     count += 1;
        // }

        pos += match rot.0 {
            Dir::R => i32::from(rot.1),
            Dir::L => -i32::from(rot.1),
        };

        if (pos % 100) == 0 {
            count += 1;
        }
    }

    count
}

fn part2(data: &[Rotation]) -> u16 {
    let mut pos = 50;
    let mut count = 0;
    for rot in data {
        let x;
        (x, pos) = spin(rot, pos);
        count += x;
    }

    count
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_spin() {
        assert_eq!(spin(&Rotation(Dir::R, 3), 5), (0, 8));
        assert_eq!(spin(&Rotation(Dir::L, 3), 5), (0, 2));
        assert_eq!(spin(&Rotation(Dir::L, 10), 5), (1, 95));
        assert_eq!(spin(&Rotation(Dir::R, 10), 95), (1, 5));
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

        let (p1, p2) = crate::evaluate_day::<Day01>(input);
        assert_eq!(p1, 3);
        assert_eq!(p2, 6);
    }
}
