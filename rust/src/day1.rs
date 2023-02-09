use std::fs;

pub fn main() {
    let inp = fs::read_to_string("../inputs/day-01/inp.txt")
        .expect("Should have been able to read the file");

    let mut parsed_data: Vec<i32> = inp
        .split("\n\n")
        .map(|itms| {
            itms.split("\n")
                .map(|itm| itm.parse::<i32>().unwrap())
                .sum()
        })
        .collect();

    parsed_data.sort();
    parsed_data.reverse();

    let part1 = parsed_data[0];
    let part2 = parsed_data[0] + parsed_data[1] + parsed_data[2];

    println!("{:?} - Part 1", part1);
    println!("{:?} - Part 2", part2);
}
