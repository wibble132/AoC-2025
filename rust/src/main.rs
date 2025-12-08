use aoc_2025_rs::*;

fn main() {
    let arg = std::env::args().next();
    if let Some(arg) = arg
        && let Ok(day) = arg.parse::<usize>()
    {
        run_day_n(day);
    } else {
        run_all_days();
    }
}

fn run_day_n(day: usize) {
    match day {
        1 => run_day::<Day01>(),
        2 => run_day::<Day02>(),
        4 => run_day::<Day04>(),
        7 => run_day::<Day07>(),
        _ => {}
    }
}

fn run_all_days() {
    for day in 1..=12 {
        run_day_n(day);
    }
}
