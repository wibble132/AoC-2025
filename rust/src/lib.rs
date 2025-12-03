#![warn(clippy::pedantic)]

mod day01;
mod day02;

pub use day01::Day01;

pub trait Day {
    const DAY_NUMBER: usize;

    fn parse(input: String) -> Self;
    fn part1(&self) -> u64;
    fn part2(&self) -> u64;
}

pub fn run_day<D: Day>() {
    let number = D::DAY_NUMBER;
    println!("Running day {number:02}");

    let input = read_input_file::<D>();
    let (p1, p2) = evaluate_day::<D>(input);

    println!("Part 1: {p1}");
    println!("Part 2: {p2}");
    println!();
}

#[must_use] 
pub fn read_input_file<D: Day>() -> String {
    let number = D::DAY_NUMBER;
    let path = format!("../inputs/day{number:02}.txt");
    std::fs::read_to_string(&path).expect(&format!("Unable to find input at {path}"))
}

#[must_use] 
pub fn evaluate_day<D: Day>(input: String) -> (u64, u64) {
    let day = D::parse(input);
    let p1 = day.part1();
    let p2 = day.part2();
    (p1, p2)
}
