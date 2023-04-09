use std::{collections::hash_map::HashMap, fs, vec};
use std::time::Instant;

pub fn main() {
    let input = fs::read_to_string("../inputs/day-05/inp.txt").expect("file to exist");


    let start = Instant::now();
    
    let (crates, moves) = input.split_once("\n\n").unwrap();

    let mut matrix: Vec<Vec<char>> = vec![];

    for lines in crates.lines() {
        let mut new_line: Vec<char> = vec![];
        for (idx, chr) in lines.chars().enumerate() {
            if ((idx + 1) % 2) == 0 {
                new_line.push(chr);
            }
        }
        matrix.push(new_line);
    }

    let mut t_matrix = transpose(matrix);

    let mut stacks = t_matrix
        .clone()
        .iter()
        .map(|a| {
            let mut b = a.to_owned();
            let key = b.pop().unwrap();
            let crates = b.into_iter().filter(|a| !a.eq(&' ')).collect::<Vec<char>>();

            (key, crates)
        })
        .collect::<HashMap<char, Vec<char>>>();


    let mvs = moves
        .lines()
        .map(|line| {
            let l = line.split_whitespace().collect::<Vec<_>>();
            let quantity: usize = l.get(1).unwrap().parse().unwrap();
            let from_stack_idx = l.get(3).unwrap().chars().next().unwrap();
            let to_stack_idx = l.get(5).unwrap().chars().next().unwrap();

            (quantity, from_stack_idx, to_stack_idx)
        })
        .collect::<Vec<_>>();

    for (quantity, from_stack_idx, to_stack_idx) in mvs.clone() {
        let from_stack = stacks.get_mut(&from_stack_idx).unwrap();
        let rng = ..quantity as usize;
        let mut crates = from_stack.drain(rng).collect::<Vec<char>>();
        let mut to_stack = stacks.get_mut(&to_stack_idx).unwrap().to_owned();
        crates.append(&mut to_stack);

        stacks.insert(to_stack_idx.clone(), crates);
    }

    for i in '1'..='9' {
        let stack = stacks.get(&i).unwrap();
        let first_cargo = stack.get(0).unwrap();
        print!("{}", first_cargo);
    }
    println!("");

    let duration = start.elapsed();

    println!("Time elapsed in expensive_function() is: {:?}", duration);
    
    // part1(input.clone());
    // part2(input);
}

fn transpose<T>(original: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(!original.is_empty());
    let mut transposed = (0..original[0].len()).map(|_| vec![]).collect::<Vec<_>>();

    for original_row in original {
        for (item, transposed_row) in original_row.into_iter().zip(&mut transposed) {
            transposed_row.push(item);
        }
    }

    transposed
}

pub fn part1(input: String) {
    println!("Day 05 - Part 01 - {:?}", input);
}

pub fn part2(input: String) {
    println!("Day 05 - Part 02 - {:?}", input);
}
