use std::fs;
use std::time::Instant;

pub fn main() {
    let input = fs::read_to_string("../inputs/day-06/inp.txt").expect("file to exist");

    let start = Instant::now();
    let result1 = calculate(&input, 4).unwrap();
    println!("{:?}: {}", start.elapsed(), result1);

    let result2 = calculate(&input, 14).unwrap();
    println!("{:?}: {}", start.elapsed(), result2);
}

fn calculate(input: &str, window: usize) -> Option<usize> {
    let indexed_chars = input.chars().collect::<Vec<char>>();

    let result = indexed_chars
        .windows(window)
        .position(|items| {
            let mut counted = vec![];

            for item in items.into_iter() {
                if counted.contains(item) {
                    return false;
                } else {
                    counted.push(*item);
                }
            }

            return true;
        });

    match result {
        Some(idx) => Some(idx + window),
        None => None,
    }
}