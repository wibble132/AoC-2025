pub struct Day04 {
    parsed: Data,
}

impl super::Day for Day04 {
    const DAY_NUMBER: usize = 4;

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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Tile {
    Empty,
    Paper,
}
#[derive(Debug, Clone)]
struct Data(Vec<Vec<Tile>>);

impl Data {
    fn width(&self) -> isize {
        self.0[0].len() as isize
    }
    fn height(&self) -> isize {
        self.0.len() as isize
    }
    fn get(&self, x: isize, y: isize) -> Tile {
        let Ok(x) = usize::try_from(x) else {
            return Tile::Empty;
        };
        let Ok(y) = usize::try_from(y) else {
            return Tile::Empty;
        };

        self.0
            .get(y)
            .and_then(|line| line.get(x))
            .copied()
            .unwrap_or(Tile::Empty)
    }
    fn neighbours(&self, x: isize, y: isize) -> impl Iterator<Item = Tile> {
        (-1..=1)
            .flat_map(move |x_offset| (-1..=1).map(move |y_offset| (x_offset, y_offset)))
            .filter(|offset| offset != &(0, 0))
            .map(move |(x_offset, y_offset)| self.get(x + x_offset, y + y_offset))
    }
    fn accessible_paper(&self) -> impl Iterator<Item = (isize, isize)> {
        let indices = (0..self.width()).flat_map(|x| (0..self.height()).map(move |y| (x, y)));
        let indices = indices.filter(|&(x, y)| self.get(x, y) == Tile::Paper);
        indices.filter(|&(x, y)| self.neighbours(x, y).filter(|t| t == &Tile::Paper).count() < 4)
    }
}

fn parse(input: &str) -> Data {
    Data(
        input
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| match c {
                        '.' => Tile::Empty,
                        '@' => Tile::Paper,
                        c => panic!("Unexpected char: {c}"),
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>(),
    )
}

fn part1(data: &Data) -> u32 {
    data.accessible_paper().count() as _
}
fn part2(data: &Data) -> u32 {
    let mut data = data.clone();
    let mut count = 0;
    let mut to_remove = Vec::new();
    loop {
        to_remove.extend(data.accessible_paper());
        if (to_remove.len()) == 0 {
            return count;
        }

        for (x, y) in &to_remove {
            data.0[*y as usize][*x as usize] = Tile::Empty;
        }

        count += to_remove.len() as u32;
        to_remove.clear()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const EXAMPLE: &str = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
";

    #[test]
    fn example() {
        assert_eq!(part1(&parse(EXAMPLE)), 13);
        assert_eq!(part2(&parse(EXAMPLE)), 43);
    }
    #[test]
    fn simple() {
        let p = parse("...\n.@.\n...\n");
        dbg!(&p);
        assert_eq!(part1(&p), 1);
        assert_eq!(part1(&p), 1);
    }
    #[test]
    fn simple_2() {
        let p = parse("..@\n.@.\n.@.\n");
        dbg!(&p);
        assert_eq!(part1(&p), 3);
        assert_eq!(part1(&p), 3);
    }
    #[test]
    fn full() {
        let p = parse("@@@\n@@@\n@@@\n");
        dbg!(&p);
        assert_eq!(part1(&p), 4);
        assert_eq!(part2(&p), 9);
    }
}
