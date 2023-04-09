use std::collections::HashSet;
use std::fs;
use std::time::SystemTime;

pub fn main() {
    let input = fs::read_to_string("../inputs/day-04/inp.txt").expect("file to exist");

    for n in 1..100 {
        let start = SystemTime::now();
        part1(input.clone());
        part2(input.clone());
        let end = SystemTime::now();
        let duration = end.duration_since(start).unwrap();
        println!("{:?}", duration);
    }
}

pub fn part1(input: String) {
    let input = range_intersect(input, |r1, r2| r1.is_subset(&r2) || r2.is_subset(&r1));

    // println!("Day 04 - Part 01 - {:?}", input);
}

pub fn part2(input: String) {
    let input = range_intersect(input, |r1, r2| !r1.is_disjoint(&r2));
    // println!("Day 04 - Part 02 - {:?}", input);
}

type P<T> = fn(HashSet<T>, HashSet<T>) -> bool;

fn range_intersect(input: String, pred: P<i32>) -> usize {
    input
        .lines()
        .filter(|line| {
            let rng: Vec<i32> = line
                .split(',')
                .flat_map(|rng| rng.split('-'))
                .map(|num| num.parse::<i32>().unwrap())
                .collect();

            let r1 = rng.get(0).unwrap().to_owned();
            let r2 = rng.get(1).unwrap().to_owned();
            let r3 = rng.get(2).unwrap().to_owned();
            let r4 = rng.get(3).unwrap().to_owned();

            let rng1: HashSet<i32> = (r1..=r2).collect();
            let rng2: HashSet<i32> = (r3..=r4).collect();

            pred(rng1, rng2)
        })
        .count()
}
