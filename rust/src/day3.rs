use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

pub fn main() {
    let input = fs::read_to_string("../inputs/day-03/inp.txt").expect("file to exist");
    part1(input.clone());
    part2(input);
}

pub fn part1(input: String) {
    let az: HashMap<char, u32> = (b'a'..=b'z')
        .map(|i| (char::from(i), (i - 96).into()))
        .chain((b'A'..=b'Z').map(|i| (char::from(i), (i - 38).into())))
        .collect();

    let ls: Vec<(&str, &str)> = input.lines().map(|l| l.split_at(l.len() / 2)).collect();

    let lsi: u32 = ls
        .iter()
        .map(|(s1, s2)| {
            (
                s1.chars().collect::<HashSet<char>>(),
                s2.chars().collect::<HashSet<char>>(),
            )
        })
        .map(|(h1, h2)| h1.intersection(&h2).next().unwrap().clone())
        .map(|i| az.get(&i).unwrap())
        .sum();

    println!("Day 03 - Part 01 - {:?}", lsi);
}

pub fn part2(input: String) {
    let az: HashMap<char, u32> = (b'a'..=b'z')
        .map(|i| (char::from(i), (i - 96).into()))
        .chain((b'A'..=b'Z').map(|i| (char::from(i), (i - 38).into())))
        .collect();

    let mut sum: u32 = 0;

    input
        .lines()
        .map(|s| s.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>()
        .chunks(3)
        .for_each(|vs| {
            let xs = vs.get(0).unwrap();
            let ys = vs.get(1).unwrap();
            let zs = vs.get(2).unwrap();

            for x in xs.iter() {
                if ys.contains(x) && zs.contains(x) {
                    sum += az.get(x).unwrap().clone();
                    break;
                }
            }
        });

    println!("Day 03 - Part 02 - {:?}", sum);
}
