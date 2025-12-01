mod day01;

trait Day {
    const DAY_NUMBER: usize;

    fn parse(input: &str) -> Self;
    fn part1(&self) -> u64;
    fn part2(&self) -> u64;
}

fn run_day<D: Day>() {
    let number = D::DAY_NUMBER;
    println!("Running day {number:02}");

    let path = format!("../inputs/day{number:02}.txt");
    let input = std::fs::read_to_string(&path).expect(&format!("Unable to find input at {path}"));
    let (p1, p2) = evaluate_day::<D>(&input);
    println!("Part 1: {p1}");
    println!("Part 2: {p2}");
}

fn evaluate_day<D: Day>(input: &str) -> (u64, u64) {
    let day = D::parse(&input);
    let p1 = day.part1();
    let p2 = day.part2();
    (p1, p2)
}

fn main() {
    run_day::<day01::Day01>()
}
