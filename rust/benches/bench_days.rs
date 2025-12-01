use aoc_2025_rs::*;
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

fn bench_day<D: Day>(c: &mut Criterion) {
    let input = read_input_file::<D>();
    let parsed = D::parse(&input);

    let num = D::DAY_NUMBER;
    let num_str = format!("{:02}", num);
    c.bench_function(&format!("day-{num_str}-parse"), |b| {
        b.iter(|| D::parse(black_box(&input)))
    });
    c.bench_function(&format!("day-{num_str}-part1"), |b| {
        b.iter(|| D::part1(black_box(&parsed)))
    });
    c.bench_function(&format!("day-{num_str}-part2"), |b| {
        b.iter(|| D::part2(black_box(&parsed)))
    });
}

criterion_group!(benches, bench_day::<Day01>);
criterion_main!(benches);
