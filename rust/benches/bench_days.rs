use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;
use aoc_2025_rs::*;

fn criterion_benchmark(c: &mut Criterion) {
    let input = read_input_file::<Day01>();
    let parsed = Day01::parse(&input);

    c.bench_function("day 01 parse", |b| b.iter(|| Day01::parse(black_box(&input))));
    c.bench_function("day 01 part1", |b| b.iter(|| Day01::part1(black_box(&parsed))));
    c.bench_function("day 01 part2", |b| b.iter(|| Day01::part2(black_box(&parsed))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
